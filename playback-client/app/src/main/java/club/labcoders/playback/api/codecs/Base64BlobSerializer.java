package club.labcoders.playback.api.codecs;

import android.util.Base64;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import java.lang.reflect.Type;

import club.labcoders.playback.api.models.Base64Blob;

public class Base64BlobSerializer implements JsonSerializer<Base64Blob> {
    @Override
    public JsonElement serialize(Base64Blob src, Type typeOfSrc, JsonSerializationContext context) {
        return new JsonPrimitive(
                Base64.encodeToString(src.getBytes(), Base64.DEFAULT)
                        .replaceAll("\n", "")
        );
    }
}
