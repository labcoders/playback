package club.labcoders.playback.concurrent;

/**
 * Channel variable.
 */
public class CVar<T> {
    private MVar<T> _data;
    private MVar<Object> _ack;

    public CVar(){
        _data = new MVar<T>();
        _ack = new MVar<Object>();
        _ack.put(null); // make _ack full
    }
    public void write(T obj) throws InterruptedException {
        _ack.take(); // make _ack empty
        _data.put(obj);
    }
    public T read() throws InterruptedException {
        T data = _data.take();
        _ack.put(null); // make _ack full
        return data;
    }
}
