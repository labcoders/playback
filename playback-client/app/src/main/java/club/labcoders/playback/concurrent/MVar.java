package club.labcoders.playback.concurrent;

public class MVar<T> {
    private T _obj;
    private boolean _full;

    public MVar(){
        _full = false;
    }
    // put: asynchronous (non-blocking)
    // Precondition: MVar must be empty
    public synchronized void put(T obj) {
        assert !_full;
        assert _obj == null;
        _obj = obj;
        _full = true;
        notify();
    }
    // take: if empty, blocks until full.
    // Removes the object and switches to empty
    public synchronized T take() throws InterruptedException {
        while (!_full)
            wait(); // may throw!

        T ret = _obj;
        _obj = null;
        _full = false;
        return ret;
    }
}