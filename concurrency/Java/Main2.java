import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.List;

/*
 * echo ./Main2.java | entr -c java /_
 */
public class Main2 {

    static int DELAY = 1;

    public static void main(String[] args) throws InterruptedException {
        System.out.printf("Booting up... (%s)\n", Thread.currentThread().getName());
        BlockingQueue<Integer> queue = new LinkedBlockingQueue<>();

        List<Consumer> consumers = List.of(
                new Consumer(queue),
                new Consumer(queue),
                new Consumer(queue));
        Producer producer = new Producer(queue, consumers.size());

        Thread producerThread = new Thread(producer);

        List<Thread> consumerThreads = consumers
                .stream()
                .map(consumer -> new Thread(consumer))
                .toList();

        producerThread.start();
        consumerThreads.forEach(Thread::start);

        producerThread.join();
        consumerThreads.forEach(t -> {
            try {
                t.join();
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        });

        System.out.printf("Total: %d (%s)\n",
                (consumers
                        .stream()
                        .mapToInt(Consumer::getTotal)
                        .sum()),
                Thread.currentThread().getName());

        System.out.printf("Shutting down... (%s)\n", Thread.currentThread().getName());
    }
}

class Producer implements Runnable {
    private BlockingQueue<Integer> queue;
    private int consumersCount;

    public Producer(BlockingQueue<Integer> queue, int consumersCount) {
        this.queue = queue;
        this.consumersCount = consumersCount;
    }

    @Override
    public void run() {
        System.out.printf("Producer Thread Running (%s)\n", Thread.currentThread().getName());

        for (int i = 0; i < 1000; i++) {
            try {
                Thread.sleep((long) (Math.random() * Main2.DELAY / 2));
                // System.out.println("Producing: " + 1);
                queue.put(1);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }

        try {
            System.out.printf("Producing poison pill for the consumers (%s)\n", Thread.currentThread().getName());
            for (int i = 0; i < consumersCount; i++) {
                queue.put(-1);
            }
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

    }
}

class Consumer implements Runnable {

    private BlockingQueue<Integer> queue;
    private int total;

    public Consumer(BlockingQueue<Integer> queue) {
        this.queue = queue;
        this.total = 0;
    }

    @Override
    public void run() {
        System.out.printf("Consumer Thread Running (%s)\n", Thread.currentThread().getName());

        while (true) {
            Integer n;
            try {
                Thread.sleep((long) (Math.random() * Main2.DELAY));
                n = queue.take();
                // System.out.println("Consuming: " + total);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            if (n == -1) {
                break;
            }
            this.total += n;
        }

        System.out.printf("Exiting Consumer Thread, total = %d (%s)\n", total, Thread.currentThread().getName());
    }

    public int getTotal() {
        return total;
    }
}
