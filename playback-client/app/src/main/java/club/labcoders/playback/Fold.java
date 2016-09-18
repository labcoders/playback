package club.labcoders.playback;

import rx.Observable;
import rx.Subscriber;
import rx.functions.Func2;

/**
 * Created by jake on 2016-09-17.
 */

public class Fold<Acc, Elem> implements Observable.Operator<Acc, Elem> {
    private final Func2<Acc, Elem, Acc> folder;
    private Acc acc;

    public Fold(Acc initial, Func2<Acc, Elem, Acc> folder) {
        this.folder = folder;
        acc = initial;
    }
    @Override
    public Subscriber<? super Elem> call(Subscriber<? super Acc> subscriber) {
        return new Subscriber<Elem>() {
            @Override
            public void onCompleted() {
                if(subscriber.isUnsubscribed())
                    return;
                subscriber.onNext(acc);
                subscriber.onCompleted();
            }

            @Override
            public void onError(Throwable e) {
                if(subscriber.isUnsubscribed())
                    return;
                subscriber.onError(e);
            }

            @Override
            public void onNext(Elem elem) {
                acc = folder.call(acc, elem);
            }
        };
    }
}
