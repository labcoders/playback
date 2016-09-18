package club.labcoders.playback;

import android.Manifest;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.media.AudioRecord;
import android.os.Environment;
import android.os.IBinder;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

import org.joda.time.DateTime;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import butterknife.BindView;
import butterknife.ButterKnife;
import butterknife.OnClick;
import club.labcoders.playback.api.ApiManager;
import club.labcoders.playback.api.PlaybackApi;
import club.labcoders.playback.api.models.AudioRecording;
import club.labcoders.playback.api.models.Base64Blob;
import club.labcoders.playback.api.models.Ping;
import rx.Observable;
import rx.Subscription;
import rx.android.schedulers.AndroidSchedulers;
import rx.schedulers.Schedulers;
import rx.subscriptions.CompositeSubscription;
import timber.log.Timber;

import static android.content.pm.PackageManager.PERMISSION_GRANTED;

public class MainActivity extends AppCompatActivity {
    private static final int FINE_LOCATION_PERMISSION_REQUEST = 0;

    @BindView(R.id.statusText)
    TextView statusText;

    @BindView(R.id.uploadButton)
    Button recordButton;

    RecordingService recordingService;
    HttpService httpService;
    private boolean recordingServiceIsBound;
    private CompositeSubscription subscriptions;
    private boolean httpServiceIsBound;
    private boolean canUseFineLocation;

    public MainActivity() {
        recordingServiceIsBound = false;
        httpServiceIsBound = false;
    }

    @OnClick(R.id.uploadButton)
    public void onRecordButtonClick() {
        double length;

        if (recordingService == null) {
            Toast.makeText(this, "No recording service.", Toast.LENGTH_SHORT).show();
        } else if (httpService == null) {
            Toast.makeText(this, "No HTTP service", Toast.LENGTH_SHORT).show();
        } else {
            short[] shorts = recordingService.getBufferedAudio();

            ByteBuffer buf = ByteBuffer.allocate(shorts.length * 2);
            buf.order(ByteOrder.nativeOrder());
            for (short s : shorts) {
                buf.putShort(s);
            }

            byte[] rawAudio = buf.array();

            Encoder enc = new Encoder();

            Observable.just(rawAudio)
                    .doOnNext(
                            bytes -> {
                                final File f = new File(
                                        Environment.getExternalStorageDirectory(),
                                        "temp.pcm"
                                );
                                Timber.d("dumping pcm to %s", f.toString());
                                try(final FileOutputStream fos
                                    = new FileOutputStream(f)) {
                                    fos.write(bytes);
                                }
                                catch(FileNotFoundException e) {
                                    Timber.e("File not found.");
                                }
                                catch(IOException e) {
                                    Timber.e("io error");
                                }
                            }
                    )
                    .lift(enc)
                    .map(encodedOutput -> encodedOutput.byteArray)
                    .lift(new BufferOperator())
//                    .lift(new MonoMuxingOperator(enc))
                    .flatMap(
                            bytes -> {
                                Timber.d(
                                        "Got muxed byte buffer length %d",
                                        bytes.length
                                );

                                final File f = new File(
                                        Environment.getExternalStorageDirectory(),
                                        "temp.mp4"
                                );
                                Timber.d("Dumping to %s.", f.getAbsolutePath());
                                try (final FileOutputStream fos
                                             = new FileOutputStream(f)) {
                                    fos.write(bytes);
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }

                                final AudioRecording rec = new AudioRecording(
                                        DateTime.now(),
                                        rawAudio.length
                                                / AudioManager.getInstance().getBytesPerSample()
                                                / AudioManager.getInstance().getSampleRate(),
                                        new Base64Blob(bytes)
                                );

                                return httpService.upload(rec);
                            }
                    )
                    .subscribeOn(Schedulers.io())
                    .observeOn(AndroidSchedulers.mainThread())
                    .subscribe(
                            id -> {
                                Timber.d("Uploaded raw with id " +
                                        "%d", id);
                                Toast.makeText(
                                        MainActivity.this,
                                        String.format(
                                                "Uploaded row with id %d",
                                                id
                                        ),
                                        Toast.LENGTH_LONG
                                ) .show();
                            },
                            err -> {
                                Timber.e("Failed to upload recording.");
                                err.printStackTrace();
                            }
                    );
        }
    }

    @OnClick(R.id.pingButton)
    public void onPingButtonClick() {
        final PlaybackApi api = ApiManager.getInstance().getApi();
        final DateTime now = DateTime.now();
        Timber.d("Now is: %s.", now);
        subscriptions.add(
                api.postPing(new Ping(now))
                        .subscribeOn(Schedulers.io())
                        .observeOn(AndroidSchedulers.mainThread())
                        .subscribe(pong -> {
                            final DateTime then = pong.getPong();
                            Timber.d("Now became: %s", then.toString());
                            Toast.makeText(
                                    MainActivity.this,
                                    String.format(
                                            "Got pong %s!",
                                            then.toString()
                                    ),
                                    Toast.LENGTH_LONG
                            ).show();
                        }, err -> {
                            Toast.makeText(this, "Could not connect to server.", Toast.LENGTH_SHORT);
                        })
        );
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        subscriptions = new CompositeSubscription();
        setContentView(R.layout.activity_main);
        ButterKnife.setDebug(true);
        ButterKnife.bind(this);

        StorageManager.initialize(this);

        Timber.d("Storage manager created. Directory at %s and cache at %s.", StorageManager.getInstance().directory(), StorageManager.getInstance().cache());

        Timber.plant(new Timber.DebugTree());

        Intent recordingIntent = new Intent(this, RecordingService.class);
        startService(recordingIntent);
        Timber.d("Started recording service.");
        bindService(recordingIntent, recordingConnection, Context.BIND_IMPORTANT);
        Timber.d("Bound recording service.");
        recordingServiceIsBound = true;

        Intent httpIntent = new Intent(this, HttpService.class);
        startService(httpIntent);
        Timber.d("Started HTTP service.");
        bindService(httpIntent, httpConnection, Context.BIND_IMPORTANT);
        Timber.d("Bound HTTP service");
        httpServiceIsBound = true;

        // Check if we have fine location permissions, and set a flag to show that we do/don't.
        if (ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) != PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(this, Manifest.permission.ACCESS_COARSE_LOCATION) != PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, FINE_LOCATION_PERMISSION_REQUEST);
        }
    }
    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onDestroy() {
        if(recordingServiceIsBound) {
            unbindService(recordingConnection);
            recordingServiceIsBound = false;
        }


        if(httpServiceIsBound) {
            unbindService(httpConnection);
            httpServiceIsBound = false;
        }
        subscriptions.unsubscribe();
        super.onDestroy();
    }

    final ServiceConnection httpConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            final HttpService.HttpServiceBinder binder = (HttpService.HttpServiceBinder) service;
            httpService = binder.getService();
            Timber.d("Assigned httpService");
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            Timber.d("Http service disconnected");
            httpService = null;
        }
    };

    final ServiceConnection recordingConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            final RecordingService.RecordingServiceBinder binder = (RecordingService.RecordingServiceBinder)service;
            recordingService = binder.getService();

            final Subscription sub = recordingService.recordingState
                    .subscribeOn(Schedulers.io())
                    .observeOn(AndroidSchedulers.mainThread())
                    .subscribe(
                            state -> {
                                if(state == AudioRecord.RECORDSTATE_RECORDING) {
                                    statusText.setText(R.string.statusRecording);
                                }
                                else {
                                    statusText.setText(R.string.statusNotRecording);
                                }
                            },
                            error -> {
                                statusText.setText(error.toString());
                            },
                            () -> {
                                statusText.setText(R.string.statusPlaceholder);
                            }
                    );
            subscriptions.add(sub);
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            Timber.d("Recording service disconnected");
            recordingService = null;
        }
    };

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case 0:
                canUseFineLocation =  (grantResults[0] == PERMISSION_GRANTED);
                break;
            default:
                throw new RuntimeException("Unexpected request code " + requestCode + " for permission request");
        }
    }
}
