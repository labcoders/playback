package club.labcoders.playback;

import android.app.Service;
import android.content.Intent;
import android.media.AudioRecord;
import android.os.Binder;

import java.nio.ByteBuffer;
import java.nio.ShortBuffer;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;

import rx.Observable;
import rx.Subscription;
import rx.schedulers.Schedulers;
import rx.subscriptions.CompositeSubscription;
import timber.log.Timber;

public class RecordingService extends Service {
    /**
     * Milliseconds between runs of the buffer GC, which removes chunks from
     * the {@link #audioBufferQueue}.
     */
    public static final long GC_PERIOD = 1000;

    /**
     * Milliseconds between emissions by the {@link #recordingState} observable.
     */
    public static final long STATE_POLL_PERIOD = 250;

    /**
     * Number of seconds in a record snapshot.
     * <p>
     * TODO: make this a configurable parameter of the application.
     */
    public static final int DEFAULT_SNAPSHOT_LENGTH = 10;

    /**
     * The queue used internally to hold buffers emitted by the
     * {@link #audioStream} observable. The internal garbage collector
     * periodically pops as many buffers as necessary from this queue in
     * order to keep the length of the queue below
     * {@link #maxAudioBufferQueueLength}.
     */
    public final Queue<short[]> audioBufferQueue;

    /**
     * A lock used for read access to the queue.
     *
     * The queue is wait-free. The microphone can continue to push data onto
     * it as much as it likes, since it is the only producer for it. However,
     * there are two consumers for the queue. One consumer is the garbage
     * collection process that drops old buffers when the queue's length
     * exceeds the configured duration. Another consumer begins when the
     * linearization process occurs. Consequently, when linearization is
     * occurring, we do not want the garbage collection process to run, as
     * that process may drop some of the buffers that we have yet to linearize.
     *
     * {@see #queueGc()}
     * {@see #audioBufferQueue}
     */
    private final ReentrantLock queueLock;

    /**
     * The maximum length of the audio buffer queue, at which point it holds
     * approximately (within <50ms on most devices)
     * {@link #snapshotLengthSeconds} seconds of audio.
     */
    private int maxAudioBufferQueueLength;
    private final CompositeSubscription subscriptions;

    /**
     * The size of the buffer that we fill when polling the microphone.
     * <p>
     * Remark: per the docs, this must be *smaller* than the buffer size in
     * {@link AudioManager}.
     */
    public int chunkBufferSize;
    /**
     * A hot observable connected to the microphone. This observable obtains
     * a value when the service is started. It emits buffers containing the
     * raw samples read from the microphone.
     */
    public Observable<short[]> audioStream;
    /**
     * A hot observable connected to the microphone. It emits the recording
     * state of the microphone every {@link #STATE_POLL_PERIOD} milliseconds.
     */
    public Observable<Integer> recordingState;
    /**
     * The duration, in seconds, of an audio snapshot created by
     * {@link #getBufferedAudio()}.
     */
    public int snapshotLengthSeconds;

    /**
     * The duration, in samples, of an audio snapshot created by
     * {@link #getBufferedAudio()}.
     */
    public int snapshotLengthSamples;

    AudioRecord audioRecord;
    /**
     * Approximates the length of the audioBufferQueue. The queue is no shorter
     * than this amount at any given time.
     */
    private AtomicInteger audioBufferQueueLength;

    public RecordingService() {
        audioBufferQueue = new ConcurrentLinkedQueue<>();
        queueLock = new ReentrantLock(true);
        subscriptions = new CompositeSubscription();
        audioBufferQueueLength = new AtomicInteger(0);
    }

    private AudioManager getAudioManager() {
        return AudioManager.getInstance();
    }

    @Override
    public void onCreate() {
        audioRecord = getAudioManager().newAudioRecord();

        snapshotLengthSeconds = DEFAULT_SNAPSHOT_LENGTH;
        snapshotLengthSamples
                = snapshotLengthSeconds * getAudioManager().getSampleRate();
        chunkBufferSize
                = getAudioManager().getBufferSize() / getAudioManager().getBytesPerSample();
        maxAudioBufferQueueLength
                = snapshotLengthSamples / chunkBufferSize;

        Timber.d(
                "Configured RecordingService: " +
                "snapshot length %d sec, %d samples " +
                "chunk buffer size %d, so %d chunks max in ABQ",
                snapshotLengthSeconds,
                snapshotLengthSamples,
                chunkBufferSize,
                maxAudioBufferQueueLength
        );

        audioStream = Observable.create(subscriber -> {
            if (audioRecord.getState() != AudioRecord.STATE_INITIALIZED) {
                subscriber.onError(
                        new RuntimeException(
                                "Failed to initialize AudioRecord"
                        )
                );
                return;
            }

            audioRecord.startRecording();
            while (audioRecord.getRecordingState()
                    == AudioRecord.RECORDSTATE_RECORDING) {
                final short[] audioBuffer = new short[chunkBufferSize];
                final int readOut = audioRecord.read(
                        audioBuffer,
                        0,
                        audioBuffer.length
                );
                switch(readOut) {
                    case AudioRecord.ERROR_INVALID_OPERATION:
                        Timber.e(
                                "Failed to read: AudioRecord not initialized."
                        );
                        subscriber.onError(
                                new RuntimeException(
                                        "AudioRecord not initialized"
                                )
                        );
                        return;
                    case AudioRecord.ERROR_DEAD_OBJECT:
                        Timber.e(
                                "Failed to read: AudioRecord died."
                        );
                        subscriber.onError(
                                new RuntimeException("AudioRecord died.")
                        );
                        return;
                    case AudioRecord.ERROR_BAD_VALUE:
                        Timber.e(
                                "Failed to read: bad values."
                        );
                        subscriber.onError(
                                new RuntimeException("AudioRecord bad values")
                        );
                        return;
                    case AudioRecord.ERROR:
                        Timber.e(
                                "Failed to read: generic error."
                        );
                        subscriber.onError(
                                new RuntimeException(
                                        "AudioRecord generic error."
                                )
                        );
                        return;
                }
                final short[] finalBuffer = new short[readOut];
                System.arraycopy(audioBuffer, 0, finalBuffer, 0, readOut);
                subscriber.onNext(finalBuffer);
            }

            Timber.d("Recording stream ended.");
            subscriber.onCompleted();
        });

        recordingState = Observable.create(subscriber -> {
            while (audioRecord.getState() != AudioRecord.STATE_UNINITIALIZED) {
                subscriber.onNext(audioRecord.getRecordingState());
                try {
                    Thread.sleep(STATE_POLL_PERIOD);
                } catch (InterruptedException e) {
                    subscriber.onError(e);
                }
            }
            Timber.d("Recording state monitor died.");
            subscriber.onCompleted();
        });
    }

    /**
     * Merges a given number of linked buffers in the audioBufferQueue into a
     * single array. New buffers may be added to the queue concurrently;
     * these buffers will not be added to the queue.
     *
     * @return A single array with the merged contents of all the linked
     * buffers in the audioBufferQueue.
     */
    private short[] linearizeQueue() {
        Timber.d("Starting audio buffer queue linearization.");
        ShortBuffer result = null;
        int processedShorts = 0;
        try {
            Timber.d("Locking audio buffer queue.");
            queueLock.lock();
            Timber.d("Doing pre-linearization GC.");
            queueGc();
            final int bufCount = audioBufferQueueLength.get();
            result = ShortBuffer.allocate(bufCount * chunkBufferSize);
            int processedBufferCount = 0;
            final long startTime = System.nanoTime();
            for (short[] buffer : audioBufferQueue) {
                if (processedBufferCount >= bufCount)
                    break;
                result.put(buffer);
                processedBufferCount++;
                processedShorts += buffer.length;
            }
            final long endTime = System.nanoTime();
            final long delta = (endTime - startTime) / (long) 1e3; // microseconds

            Timber.d("Audio buffer queue linearized in %d microseconds.",
                    delta);
        } finally {
            if (queueLock.isHeldByCurrentThread())
                queueLock.unlock();
        }
        final short[] finalResult = new short[processedShorts];
        System.arraycopy(
                result.array(),
                0,
                finalResult,
                0,
                finalResult.length
        );
        return finalResult;
    }

    /**
     * Returns a buffer of the last {@link #snapshotLengthSeconds} seconds of
     * audio.
     *
     * @return The recorded audio, if any.
     */
    public short[] getBufferedAudio() {
        // snapshotLengthSamples is the number of samples that make up a
        // piece of
        // audio with length snapshotLengthSeconds; dividing by chunkBufferSize
        // gives us the number of buffers we need to pop to get a sample
        // array whose represented audio has that length in seconds.
        return linearizeQueue();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        final Subscription producer = audioStream
                .subscribeOn(Schedulers.io())
                .observeOn(Schedulers.io())
                .subscribe(shorts -> {
                    audioBufferQueue.add(shorts);
                    audioBufferQueueLength.incrementAndGet();
                });
        subscriptions.add(producer);

        final Observable<Void> gcTimer = Observable.create(subscriber -> {
            while (true) {
                subscriber.onNext(null);
                try {
                    Thread.sleep(GC_PERIOD);
                } catch (InterruptedException e) {
                    subscriber.onError(e);
                    return;
                }
            }
        });
        subscriptions.add(
                gcTimer.subscribeOn(Schedulers.io())
                        .observeOn(Schedulers.io())
                        .subscribe(aVoid -> queueGc())
        );
        return START_STICKY;
    }

    /**
     * Perform a garbage collection of the audio buffer queue.
     * <p>
     * This will acquire the queue lock, so that other garbage collection
     * processes or linearization processes cannot be executing concurrently.
     */
    private void queueGc() {
        int collected = 0;
        while (audioBufferQueueLength.get() >
                maxAudioBufferQueueLength) {
            try {
                queueLock.lock();
                audioBufferQueue.remove();
                audioBufferQueueLength.decrementAndGet();
                collected++;
            } finally {
                queueLock.unlock();
            }
        }
        if (collected > 0) {
            Timber.d("Garbage collected %d chunks.",
                    collected);
        }
    }

    @Override
    public void onDestroy() {
        subscriptions.unsubscribe();
        audioRecord.stop();
        audioRecord.release();
        Timber.d("RecordingService destroyed.");
    }

    @Override
    public RecordingServiceBinder onBind(Intent intent) {
        return new RecordingServiceBinder();
    }

    class RecordingServiceBinder extends Binder {
        RecordingService getService() {
            return RecordingService.this;
        }
    }
}
