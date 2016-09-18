package club.labcoders.playback.api.models;

/**
 * Class that implements JSON de/serialization of binary data via base64
 * encoding.
 */
public class Base64Blob {
    private byte[] bytes;

    public Base64Blob() {
        bytes = null;
    }

    public Base64Blob(byte[] bytes) {
        this.bytes = bytes;
    }

    public byte[] getBytes() {
        return bytes;
    }

    public void setBytes(byte[] bytes) {
        this.bytes = bytes;
    }
}
