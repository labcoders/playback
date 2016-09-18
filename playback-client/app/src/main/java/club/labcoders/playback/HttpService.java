package club.labcoders.playback;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.support.annotation.Nullable;

import club.labcoders.playback.api.ApiManager;
import club.labcoders.playback.api.PlaybackApi;
import club.labcoders.playback.api.models.AudioRecording;
import rx.Observable;
import timber.log.Timber;

public class HttpService extends Service {
    private PlaybackApi api;

    public HttpService() {
        api = ApiManager.getInstance().getApi();
        Timber.d("Http service constructor called");
    }

    public Observable<Integer> upload(AudioRecording rec) {
        Timber.d("Recording upload started.");
        return api.uploadRecording(rec);
    }

    public Observable<AudioRecording> get(Integer id) {
        Timber.d("Recoding retrieval started.");
        return api.getRecording(id);
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return new HttpServiceBinder();
    }

    public class HttpServiceBinder extends Binder {
        public HttpService getService() { return HttpService.this; }
    }

    @Override
    public void onDestroy() {
        Timber.d("Destroyed HTTP service.");
        super.onDestroy();
    }
}
