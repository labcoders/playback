package club.labcoders.playback.api.models;

import org.joda.time.DateTime;

public class Ping {
    private DateTime ping;

    public Ping() {
    }

    public Ping(DateTime ping) {
        this.ping = ping;
    }

    public DateTime getPing() {
        return ping;
    }

    public void setPing(DateTime ping) {
        this.ping = ping;
    }
}
