package club.labcoders.playback.api;

import club.labcoders.playback.api.models.AudioRecording;
import club.labcoders.playback.api.models.Ping;
import club.labcoders.playback.api.models.Pong;
import retrofit2.http.Body;
import retrofit2.http.GET;
import retrofit2.http.POST;
import retrofit2.http.Path;
import rx.Observable;

public interface PlaybackApi {
    @GET("uploads/{id}")
    Observable<AudioRecording> getRecording(@Path("id") int id);

    @POST("uploads")
    Observable<Integer> uploadRecording(@Body AudioRecording recording);

    @POST("ping")
    Observable<Pong> postPing(@Body Ping ping);
}