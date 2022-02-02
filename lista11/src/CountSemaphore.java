//Jakub Bednarek

import java.util.concurrent.Semaphore;

//Zad 2b
class IntCellSemaphore {
    private int n = 0;

    public int getN() {
        return n;
    }

    public void setN(int n) {
        this.n = n;
    }
}

public class CountSemaphore extends Thread{
    private static IntCellSemaphore n = new IntCellSemaphore();
    private static Semaphore s = new Semaphore(1);

    @Override
    public void run() {
        int temp;
        for(int i = 0; i < 200000; i++){
            try{
                s.acquire();
            } catch(InterruptedException e) {}

            temp = n.getN();
            n.setN(temp + 1);
            s.release();
        }
    }

    public static void main(String[] args) {
        CountSemaphore p = new CountSemaphore();
        CountSemaphore q = new CountSemaphore();
        p.start();
        q.start();
        try { p.join(); q.join(); }
        catch (InterruptedException e) { }
        System.out.println("The value of n is " + n.getN());
    }
}
