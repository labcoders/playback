package club.labcoders.playback.api.models;


public class GeographicalPosition {
    private double latitude;
    private double longitude;

    public GeographicalPosition() {
        latitude = 0;
        longitude = 0;
    }

    public GeographicalPosition(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    public double getLatitude() {
        return latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }
}
