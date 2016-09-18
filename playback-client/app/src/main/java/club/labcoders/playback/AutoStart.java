package club.labcoders.playback;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import timber.log.Timber;

public class AutoStart extends BroadcastReceiver {
    public AutoStart() {
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        if(filterIntent(intent)) {
            final Intent recordingIntent = new Intent(context, RecordingService.class);
            context.startService(recordingIntent);
            Timber.i("Autostarted recording service from intent %s.", intent.getAction());
        }
        else {
            Timber.i("Received intent %s which matches none in the filter.", intent.getAction());
        }
    }

    private boolean filterIntent(final Intent intent) {
        return intent.getAction().equals("android.intent.action.BOOT_COMPLETED") ||
                intent.getAction().equals("android.intent.action.QUICKBOOT_POWERON");
    }
}
