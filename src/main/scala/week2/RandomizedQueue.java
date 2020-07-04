package week2;

import edu.princeton.cs.algs4.StdRandom;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class RandomizedQueue<Item> implements Iterable<Item> {
    private Object[] elements = new Object[16];
    private int N;

    // construct an empty randomized queue
    public RandomizedQueue() {

    }

    // is the randomized queue empty?
    public boolean isEmpty() { return N == 0; }

    // return the number of items on the randomized queue
    public int size() { return N; }

    // add the item
    public void enqueue(Item item) {
        if (item == null) throw new IllegalArgumentException();
        if (N == elements.length) resize(N * 2);
        for (int i = 0; i < elements.length; i++) {
            Object now = elements[i];
            if (now == null) {
                elements[i] = item;
                break;
            }
        }
        N += 1;
    }

    private void resize(int max) {
        //System.out.println("Resizing from " + elements.length + " to " + max);
        Object[] temp = new Object[max];
        int now = 0;
        for (Object ele : elements) {
            if (ele != null) temp[now++] = ele;
        }
        elements = temp;
    }

    // remove and return a random item
    public Item dequeue() {
        if (N == 0) throw new NoSuchElementException("Queue is Empty, can't dequeue");
        Object now = null;
        int index = -1;
        while (now == null) {
            index = StdRandom.uniform(elements.length);
            now = elements[index];
        }
        //@SuppressWarnings("unchecked")
        Item element = (Item) now;
        elements[index] = null;
        N -= 1;
        return element;
    }

    // return a random item (but do not remove it)
    public Item sample() {
        if (N == 0) throw new NoSuchElementException("Queue is Empty, no element to choose");
        Object now = null;
        int index;
        while (now == null) {
            index = StdRandom.uniform(elements.length);
            now = elements[index];
        }
        //@SuppressWarnings("unchecked")
        Item element = (Item) now;
        return element;
    }

    // return an independent iterator over items in random order
    public Iterator<Item> iterator() {
        return new Iterator<Item>() {
            private int produced = 0;
            private int nowIndex = 0;
            @Override
            public boolean hasNext() {
                return produced != N;
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException("Queue iterator don't support remove method.");
            }

            @Override
            //@SuppressWarnings("unchecked")
            public Item next() {
                for (int i = nowIndex; i < elements.length; i++) {
                    Object now = elements[i];
                    if (now != null) {
                        //System.out.println("Find one " + now + " nowIndex is " + nowIndex + " " +
                        //        " i is " + i +" find " + produced);
                        produced += 1;
                        nowIndex = i + 1;
                        return (Item) now;
                    }
                }
                throw new NoSuchElementException();
            }
        };
    }

    // unit testing (required)
    public static void main(String[] args) {
        RandomizedQueue<String> a = new RandomizedQueue<>();
        a.enqueue("H");
        a.enqueue("E");
        a.enqueue("L");
        a.enqueue("L");
        a.enqueue("O");
        System.out.println("Dequene" + a.dequeue());
        System.out.println("Dequene" + a.dequeue());
        a.enqueue("Z");
        for (Object e: a.elements) {
            System.out.println(e);
        }
        a.forEach(System.out::println);
    }

}