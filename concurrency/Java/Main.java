import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.Semaphore;

/*
 * Goal: Make many workers compute some numbers then make sure they come back in
 * a timely fashion to add them up.
 *
 * Access needs to be "synchronized" as soon as more than one thread (per class)
 * is started, since no coordination happens from there on (unless specified).
 *
 * https://www.w3resource.com/java-exercises/multithreading/index.php
 *
 * echo ./Main.java | entr -c java /_
 */
class Main {

    public static void main(String[] args) throws InterruptedException {
        Counter counter1 = new Counter();
        Counter2 counter2 = new Counter2();
        Counter3 counter3 = new Counter3();
        Counter4 counter4 = new Counter4();
        Counter5 counter5 = new Counter5();

        List<Thread> threads = new ArrayList<>();

        for (int i = 0; i < 3; i++) {
            // Thread thread = new Thread(() -> {
            // for (int j = 0; j < 1000; j++) {
            // counter1.inc();
            // counter2.inc();
            // counter3.inc();
            // counter4.inc();
            // counter5.inc();
            // }
            // });
            // thread.start();
            // threads.add(thread);

            Thread thread = Thread.ofVirtual().start(() -> {
                for (int j = 0; j < 100000; j++) {
                    counter1.inc();
                    counter2.inc();
                    counter3.inc();
                    counter4.inc();
                    counter5.inc();
                }
            });
            threads.add(thread);
        }

        for (Thread t : threads) {
            t.join();
        }

        System.out.printf("(Java threads) Counter1 is: %7d\n", counter1.getVal());
        System.out.printf("(Java threads) Counter2 is: %7d\n", counter2.getVal());
        System.out.printf("(Java threads) Counter3 is: %7d\n", counter3.getVal());
        System.out.printf("(Java threads) Counter4 is: %7d\n", counter4.getVal());
        System.out.printf("(Java threads) Counter5 is: %7d\n", counter5.getVal());

        System.out.println("(Java threads) Total is:    "
                + (counter1.getVal() + counter2.getVal() + counter3.getVal() +
                        counter4.getVal() + counter5.getVal()));
    }
}

class Counter {
    private int val;

    Counter() {
        this.val = 0;
    }

    synchronized void inc() {
        val++;
    }

    public int getVal() {
        return val;
    }
}

class Counter2 {
    private AtomicInteger val;

    Counter2() {
        this.val = new AtomicInteger(0);
    }

    void inc() {
        val.incrementAndGet();
    }

    public int getVal() {
        return val.get();
    }
}

class Counter3 {
    private AtomicReference<Integer> val;

    Counter3() {
        this.val = new AtomicReference<>(0);
    }

    void inc() {
        val.getAndUpdate(i -> i + 1);
    }

    public int getVal() {
        return val.get();
    }
}

class Counter4 {
    private int val;
    private ReentrantLock mutex;

    Counter4() {
        this.val = 0;
        this.mutex = new ReentrantLock();
    }

    void inc() {
        mutex.lock();
        try {
            val++;
        } finally {
            mutex.unlock();
        }
    }

    public int getVal() {
        return val;
    }
}

class Counter5 {
    private int val;
    private Semaphore semaphore;

    Counter5() {
        this.val = 0;
        this.semaphore = new Semaphore(1);
    }

    void inc() {
        try {
            semaphore.acquire();
            val++;
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        } finally {
            semaphore.release();
        }
    }

    public int getVal() {
        return val;
    }
}