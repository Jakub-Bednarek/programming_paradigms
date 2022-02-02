//Jakub Bednarek

//Zad 2a
class IntCellMonitors {
    private int n = 0;
    private boolean set = true;

    public synchronized int getN() {
        while(!set){
            try{
                wait();
            } catch(InterruptedException e) {}
        }

        set = false;
        notifyAll();

        return n;
    }

    public synchronized void setN(int n) {
        if(set){
            try{
                wait();
            } catch(InterruptedException e) {}
        }

        this.n = n;
        set = true;
        notifyAll();
    }
}

class CountMonitors extends Thread {
    private static IntCellMonitors n = new IntCellMonitors();

    @Override public void run() {
        int temp;
        for (int i = 0; i < 200000; i++) {
            temp = n.getN();
            n.setN(temp + 1);
        }
    }

    public static void main(String[] args) {
        CountMonitors p = new CountMonitors();
        CountMonitors q = new CountMonitors();
        p.start();
        q.start();
        try { p.join(); q.join(); }
        catch (InterruptedException e) { }
        System.out.println("The value of n is " + n.getN());
    }
}