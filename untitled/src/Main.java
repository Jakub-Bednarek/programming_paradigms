//Jakub Bednarek
//Zadanie 1

public class Main {
    public static void main(String[] args) {
        MyQueue<Integer> queue = new CyclicQueue<>(3);

        queue.enqueue(15);
        queue.enqueue(25);
        queue.enqueue(35);

        try {
            queue.enqueue(1000);
        }
        catch(FullException e){
            System.out.println(e.getMessage());
            e.printStackTrace();
        }

        System.out.println(queue.first());
        queue.dequeue();

        System.out.println(queue.first());
        queue.dequeue();

        System.out.println(queue.first());
        queue.dequeue();

        try{
            System.out.println(queue.first());
        } catch(EmptyException e){
            System.out.println(e.getMessage());
            e.printStackTrace();
        }

        queue.enqueue(45);
        queue.enqueue(55);

        System.out.println(queue.first());
        queue.dequeue();

        System.out.println(queue.first());
        queue.dequeue();
    }
}
