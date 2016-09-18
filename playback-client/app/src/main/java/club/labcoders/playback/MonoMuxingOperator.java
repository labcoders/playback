package club.labcoders.playback;

import android.media.MediaCodec;
import android.media.MediaFormat;
import android.media.MediaMuxer;

import org.joda.time.DateTime;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;

import rx.Observable;
import rx.Subscriber;
import timber.log.Timber;

/**
 * Rx operator that consumes all byte buffers it receives, concatenating them.
 * Upon receiving the onComplete signal from upstream, sends the accumulated
 * buffer downstream and completes immediately after.
 */
public class MonoMuxingOperator implements Observable.Operator<byte[], EncodedOutput> {

    MediaMuxer mux;
    int trackID;
    File tempFile;
    boolean initializedMuxer;
    Encoder encoder;

    public MonoMuxingOperator(Encoder enc) {
        encoder = enc;
        initializedMuxer = false;
    }

    /* Returns a unique, temporary file name */
    private String tempFileId() {
        return DateTime.now().toString() + "temp.m4a";
    }

    @Override
    public Subscriber<? super EncodedOutput>
    call(Subscriber<? super byte[]> subscriber) {
        return new Subscriber<EncodedOutput>() {
            @Override
            public void onCompleted() {
                if (subscriber.isUnsubscribed()) {
                    return;
                }

                try {
                    RandomAccessFile audioFile = new RandomAccessFile(tempFile, "r");
                    byte[] finalAudio = new byte[(int) audioFile.length()];
                    audioFile.readFully(finalAudio);

                    subscriber.onNext(finalAudio);
                    subscriber.onCompleted();

                    audioFile.close();
                    mux.stop();
                    mux.release();
                    mux = null;
                    tempFile.delete();

                    initializedMuxer = false;

                } catch (IOException e) {
                    onError(e);
                }
            }

            @Override
            public void onError(Throwable e) {
                if(subscriber.isUnsubscribed())
                    return;
                subscriber.onError(e);
            }

            @Override
            public void onNext(EncodedOutput encoded) {
                if(subscriber.isUnsubscribed()) {
                    return;
                }
                if (!initializedMuxer) {
                    try {
                        tempFile = File.createTempFile(tempFileId(), null);
                        mux = new MediaMuxer(tempFile.getAbsolutePath(), MediaMuxer.OutputFormat.MUXER_OUTPUT_MPEG_4);
                    } catch (IOException e) {
                        Timber.e("Failed to initialize muxer.");
                        e.printStackTrace();
                        return;
                    }

                    Timber.d("Created new temp file at %s", tempFile.getAbsolutePath());

                    trackID = mux.addTrack(encoder.codec.getOutputFormat());
                    mux.start();
                    Timber.d("Started muxer.");
                    initializedMuxer = true;
                }

                try {
                    mux.writeSampleData(trackID, ByteBuffer.wrap(encoded.byteArray), encoded.bufferInfo);
                } catch (Exception e) {
                    onError(e);
                }
            }
        };
    }
}
