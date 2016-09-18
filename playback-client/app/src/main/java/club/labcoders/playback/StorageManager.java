package club.labcoders.playback;

import android.content.Context;

import java.io.File;

public class StorageManager {
    private static File BASE_DIR;
    private static File CACHE;
    private static Context APP_CONTEXT;
    private static StorageManager INSTANCE;

    private StorageManager(Context ctx) {
            APP_CONTEXT = ctx;
            BASE_DIR = ctx.getFilesDir();
            CACHE = ctx.getCacheDir();
    }

    public static void initialize(Context ctx) {
        if (INSTANCE != null) {
            INSTANCE = new StorageManager(ctx);
        }
    }

    public static StorageManager getInstance() {
        return INSTANCE;
    }

    public static File directory() {
        return BASE_DIR;
    }
    public static File cache() {
        return CACHE;
    }
}
