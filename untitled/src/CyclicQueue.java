//Jakub Bednarek
//Zadanie 1

import java.util.ArrayList;

public class CyclicQueue<E> implements MyQueue<E>{
    private final ArrayList<E> queue;
    private int r;
    private int f;
    private final int size;

    public CyclicQueue(int size)
    {
        this.queue = new ArrayList<>(size + 1);
        this.size = size + 1;
        this.r = 0;
        this.f = 0;
    }

    @Override
    public void enqueue(E x) throws FullException {
        if(isFull()){
            throw new FullException("Queue is full!");
        }
        else {
            if(queue.size() < size)
                queue.add(r, x);
            else
                queue.set(r, x);

            r = (r + 1) % size;
        }
    }

    @Override
    public void dequeue() {
        if(!isEmpty()) {
            f = (f + 1) % size;
        }
    }

    @Override
    public E first() throws EmptyException {
        if(isEmpty())
            throw new EmptyException("Queue is empty!");

        return queue.get(f);
    }

    @Override
    public boolean isEmpty() {
        return f == r;
    }

    @Override
    public boolean isFull() {
        return (r + 1) % size == f;
    }
}
