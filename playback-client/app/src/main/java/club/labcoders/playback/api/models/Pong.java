package club.labcoders.playback.api.models;

import org.joda.time.DateTime;

/**
 * Created by jake on 2016-09-16.
 */

public class Pong {
    private DateTime pong;

    public Pong() {
    }

    public DateTime getPong() {
        return pong;
    }

    public void setPong(DateTime pong) {
        this.pong = pong;
    }
}
