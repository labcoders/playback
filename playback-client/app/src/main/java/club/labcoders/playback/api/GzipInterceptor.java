package club.labcoders.playback.api;

import java.io.IOException;

import okhttp3.Interceptor;
import okhttp3.MediaType;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okio.Buffer;
import okio.BufferedSink;
import okio.GzipSink;
import okio.Okio;
import timber.log.Timber;

/**
 * Gzips requests before sending them to the server.
 */
public class GzipInterceptor implements Interceptor {
    @Override
    public Response intercept(final Chain chain) throws IOException {
        final Request originalRequest = chain.request();
        if(originalRequest == null ||
                originalRequest.header("Content-Encoding") != null)
            return chain.proceed(originalRequest);
        final Request compressedRequest = originalRequest.newBuilder()
                .header("Content-Encoding", "gzip")
                .method(
                        originalRequest.method(),
                        forceContentLength(gzip(originalRequest.body()))
                )
                .build();
        Timber.d(
                "Gzipped request: %d B -> %d B",
                originalRequest.body().contentLength(),
                compressedRequest.body().contentLength()
        );
        return chain.proceed(compressedRequest);
    }

    private RequestBody forceContentLength(RequestBody body) throws IOException {
        final Buffer buffer = new Buffer();
        body.writeTo(buffer);
        return new RequestBody() {
            @Override
            public MediaType contentType() {
                return body.contentType();
            }

            @Override
            public long contentLength() throws IOException {
                return buffer.size();
            }

            @Override
            public void writeTo(BufferedSink sink) throws IOException {
                sink.write(buffer.snapshot());
            }
        };
    }

    private RequestBody gzip(final RequestBody body) {
        return new RequestBody() {
            @Override
            public MediaType contentType() {
                return body.contentType();
            }

            @Override
            public long contentLength() throws IOException {
                return -1;
            }

            @Override
            public void writeTo(final BufferedSink sink) throws IOException {
                BufferedSink gzipSink = Okio.buffer(new GzipSink(sink));
                body.writeTo(gzipSink);
                gzipSink.close();
            }
        };
    }
}
