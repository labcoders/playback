package club.labcoders.playback.api.codecs;

import android.util.Base64;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;

import java.lang.reflect.Type;

import club.labcoders.playback.api.models.Base64Blob;

public class Base64BlobDeserializer implements JsonDeserializer<Base64Blob> {
    @Override
    public Base64Blob deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
            throws JsonParseException {
        return new Base64Blob(
                Base64.decode(
                        json.getAsJsonPrimitive().getAsString(),
                        Base64.DEFAULT
                )
        );
    }
}
