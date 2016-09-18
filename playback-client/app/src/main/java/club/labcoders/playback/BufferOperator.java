package club.labcoders.playback;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import rx.Observable;
import rx.Subscriber;

/**
 * Rx operator that consumes all byte buffers it receives, concatenating them.
 * Upon receiving the onComplete signal from upstream, sends the accumulated
 * buffer downstream and completes immediately after.
 */
public class BufferOperator implements Observable.Operator<byte[], byte[]> {
    final ByteArrayOutputStream output;

    public BufferOperator() {
        output = new ByteArrayOutputStream();
    }

    @Override
    public Subscriber<? super byte[]>
    call(Subscriber<? super byte[]> subscriber) {
        return new Subscriber<byte[]>() {
            @Override
            public void onCompleted() {
                if(subscriber.isUnsubscribed())
                    return;
                subscriber.onNext(output.toByteArray());
                subscriber.onCompleted();
            }

            @Override
            public void onError(Throwable e) {
                if(subscriber.isUnsubscribed())
                    return;
                subscriber.onError(e);
            }

            @Override
            public void onNext(byte[] bytes) {
                if(subscriber.isUnsubscribed())
                    return;
                try {
                    output.write(bytes);
                } catch (IOException e) {
                    onError(e);
                }
            }
        };
    }
}
