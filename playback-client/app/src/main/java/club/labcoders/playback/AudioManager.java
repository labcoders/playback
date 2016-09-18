package club.labcoders.playback;

import android.media.AudioFormat;
import android.media.AudioRecord;
import android.media.MediaRecorder;

import timber.log.Timber;

/**
 * Singleton class for our audio settings.
 */
public class AudioManager {
    /**
     * The possible values to use as a sample rate for the microphone.
     */
    public static final int[] SAMPLE_RATES = {
            44100,
//            22050,
//            11025,
//            8000,
    };

    public static final int[] AUDIO_FORMATS = {
            AudioFormat.ENCODING_PCM_16BIT,
//            AudioFormat.ENCODING_PCM_8BIT,
//            AudioFormat.ENCODING_PCM_FLOAT,
    };

    public static final int[] CHANNEL_CONFIGS = {
            AudioFormat.CHANNEL_IN_MONO,
//            AudioFormat.CHANNEL_IN_STEREO,
    };

    private static AudioManager instance = null;

    /**
     * The size of the audio buffers used internally by Android in our audio
     * recorder.
     */
    private final int bufferSize;

    private final int channelConfig;
    private final int sampleRate;
    private final int audioFormat;

    private AudioManager(
            int bufferSize,
            int sampleRate,
            int audioFormat,
            int channelConfig) {
        this.bufferSize = bufferSize;
        this.sampleRate = sampleRate;
        this.channelConfig = channelConfig;
        this.audioFormat = audioFormat;
    }

    public int getChannelConfig() {
        return channelConfig;
    }

    public int getSampleRate() {
        return sampleRate;
    }

    public int getAudioFormat() {
        return audioFormat;
    }

    public static AudioManager getInstance() {
        if(instance == null)
            instance = makeInstance();
        return instance;
    }

    public AudioRecord newAudioRecord() {
        return new AudioRecord(
                MediaRecorder.AudioSource.MIC,
                sampleRate,
                channelConfig,
                audioFormat,
                bufferSize
        );
    }

    public int getBytesPerSample() {
        switch(audioFormat) {
            case AudioFormat.ENCODING_PCM_8BIT:
                return 1;
            case AudioFormat.ENCODING_PCM_16BIT:
                return 2;
            case AudioFormat.ENCODING_PCM_FLOAT:
                return 4;
            default:
                throw new RuntimeException(
                        String.format(
                                "Unknown audio format %d",
                                audioFormat
                        )
                );
        }
    }

    private static AudioManager makeInstance() {
        int bufferSize = 0;
        AudioRecord audioRecord = null;
        int goodSampleRate = 0;
        int goodAudioFormat = 0;
        int goodChannelConfig = 0;
        boolean initialized = false;

        for(int sampleRate : SAMPLE_RATES) {
            for(int audioFormat : AUDIO_FORMATS) {
                for(int channelConfig : CHANNEL_CONFIGS) {
                    bufferSize = AudioRecord.getMinBufferSize(
                            sampleRate,
                            channelConfig,
                            audioFormat
                    );
                    audioRecord = new AudioRecord(
                            MediaRecorder.AudioSource.VOICE_COMMUNICATION,
                            sampleRate,
                            channelConfig,
                            audioFormat,
                            bufferSize
                    );
                    if (audioRecord.getState() == AudioRecord.STATE_INITIALIZED) {
                        initialized = true;
                        goodSampleRate = sampleRate;
                        goodAudioFormat = audioFormat;
                        goodChannelConfig = channelConfig;
                        break;
                    }
                }
            }
        }

        if(!initialized)
            throw new RuntimeException("Failed to initialize AudioRecord");

        Timber.d(
                "Successfully initialized audio manager. " +
                        "Buffer size %d; sample rate %d; audio format %d; " +
                        "channel config %d"
                ,
                bufferSize,
                goodSampleRate,
                goodAudioFormat,
                goodChannelConfig
        );

        return new AudioManager(
                bufferSize,
                goodSampleRate,
                goodAudioFormat,
                goodChannelConfig
        );
    }

    public int getBufferSize() {
        return bufferSize;
    }
}
