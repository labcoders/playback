package club.labcoders.playback;

import android.media.AudioFormat;
import android.media.MediaCodec;
import android.media.MediaCodecInfo;
import android.media.MediaFormat;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import club.labcoders.playback.concurrent.CVar;
import rx.Observable;
import rx.Subscriber;
import timber.log.Timber;

/**
 * Class for wrapping Android's built-in encoding facilities with RxJava.
 */
public class Encoder implements Observable.Operator<EncodedOutput, byte[]> {
    private final int inputSampleRate;
    private final int channelConfig;
    MediaCodec codec;

    boolean hasError = false;
    Throwable error = null;

    private boolean codecInitialized;

    public final static String MIME_TYPE = MediaFormat.MIMETYPE_AUDIO_AAC;
    private boolean sendEOS;

    public Encoder() {
        this(
                AudioManager.getInstance().getSampleRate(),
                AudioManager.getInstance().getChannelConfig()
        );
    }

    public Encoder(int inputSampleRate, int channelConfig) {
        codecInitialized = false;
        sendEOS = false;
        this.inputSampleRate = inputSampleRate;
        this.channelConfig = channelConfig;
        Timber.d("Created Encoder.");
    }

    private void initCodec(int sampleRate, int channelConfig) {
        try {
            codec = MediaCodec.createEncoderByType(MIME_TYPE);
        } catch (IOException e) {
            Timber.e(
                    "Could not create MediaCodec with mimetype: %s",
                    MIME_TYPE
            );
        }

        MediaFormat format = MediaFormat.createAudioFormat(
                MIME_TYPE,
                sampleRate,
                channelConfig
        );

        format.setInteger(MediaFormat.KEY_BIT_RATE, 128000);
        format.setInteger(
                MediaFormat.KEY_AAC_PROFILE,
                MediaCodecInfo.CodecProfileLevel.AACObjectLC
        );
        format.setInteger(MediaFormat.KEY_MAX_INPUT_SIZE, 1000000);
        format.setInteger(MediaFormat.KEY_CHANNEL_COUNT, 1);

        codec.configure(format, null, null, MediaCodec.CONFIGURE_FLAG_ENCODE);
        codec.start();

        Timber.d("Initialized codec.");
        codecInitialized = true;
    }

    @Override
    public Subscriber<? super byte[]>
    call(Subscriber<? super EncodedOutput> subscriber) {

        Subscriber sub = new Subscriber<byte[]>(subscriber) {
            @Override
            public void onCompleted() {
                Timber.d("onComplete upstream to encoder called.");
                sendEOS = true;
                onNext(new byte[0]);

                codec.stop();
                codec.release();
                codecInitialized = false;

                subscriber.onCompleted();
            }

            @Override
            public void onError(Throwable e) {
                if(!subscriber.isUnsubscribed()) {
                    subscriber.onError(e);
                }
            }

            @Override
            public void onNext(byte[] bytes) {
                // Order bytes by native endian-ness
                if(subscriber.isUnsubscribed()) {
                    return;
                }
                if (!codecInitialized) {
                    initCodec(inputSampleRate, channelConfig);
                }
                int pointer = 0;
                int finalSize = 0;
                while (pointer < bytes.length) {
                    Timber.d("Processed %d/%d", pointer, bytes.length);
                    int inIdx = -1;
                    int inc = 0;

                    int getInputBufferTries = 3;

                    while (inIdx < 0 && getInputBufferTries > 0) {
                        inIdx = codec.dequeueInputBuffer(1000);
                        if (inIdx >= 0) {
                            ByteBuffer inBuffer = codec.getInputBuffer(inIdx);
                            int capacity = inBuffer.capacity();

                            // inc is number of bytes that we are going
                            // to write into the inputBuffer
                            inc = Math.min(bytes.length - pointer, capacity);

                            inBuffer.put(bytes, pointer, inc);
                            pointer += inc;
                            if (sendEOS) {
                                codec.queueInputBuffer(inIdx, 0, inc, 0, MediaCodec.BUFFER_FLAG_END_OF_STREAM);
                            } else {
                                codec.queueInputBuffer(inIdx, 0, inc, 0, 0);
                            }
                        }
                        getInputBufferTries -= 1;
                    }

                    int outIdx;

                    int getOutputBufferTries = 3;
                    while (getOutputBufferTries > 0) {
                        MediaCodec.BufferInfo newInfo = new MediaCodec.BufferInfo();
                        outIdx = codec.dequeueOutputBuffer(newInfo, 1000);
                        if (outIdx >= 0) {
                            ByteBuffer outBuffer = codec.getOutputBuffer(outIdx);
                            finalSize += outBuffer.remaining();
                            byte[] outArray = new byte[outBuffer.remaining()];
                            outBuffer.get(outArray);

                            EncodedOutput out = new EncodedOutput(outArray, newInfo);

                            subscriber.onNext(out);
                            codec.releaseOutputBuffer(outIdx, newInfo.presentationTimeUs);
                        } else {
                            switch (outIdx) {
                                case -1:
                                    Timber.e("Timed out while waiting for output buffer.");
                                    break;
                                case -2:
                                    Timber.d("Output format changed");
                                    break;
                                case -3:
                                    Timber.d("Output buffers changed (DEPRECATED).");
                                    break;
                                default:
                                    Timber.e("THE END IS NIGH");
                            }
                            getOutputBufferTries -= 1;
                        }
                    }
                }
                if (sendEOS) {
                    Timber.d("Sent EOS.");
                } else {
                    Timber.d("Encoded %d bytes, and wrote %d bytes to observer.", bytes.length, finalSize);
                }
            }
        };

        return sub;
    }
}
