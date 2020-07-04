package week2;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class Deque<Item> implements Iterable<Item> {
    private Object[] elements = new Object[16];
    private int head;
    private int tail;

    private void doubleCapacity() {
        assert head == tail;
        int p = head;
        int n = elements.length;
        int r = n - p; // number of elements to the right of p
        int newCapacity = n << 1;
        if (newCapacity < 0)
            throw new IllegalStateException("Sorry, deque too big");
        Object[] a = new Object[newCapacity];
        System.arraycopy(elements, p, a, 0, r);
        System.arraycopy(elements, 0, a, r, p);
        elements = a;
        head = 0;
        tail = n;
    }
    // construct an empty deque
    public Deque() {

    }

    // is the deque empty?
    public boolean isEmpty() {
        return head == tail;
    }

    // 数组长度必定为 2^n，其 - 1 必定为 n 个 1 的二进制
    // 求和时，1 & 1 = 1，1 & 0 = 0，0 & 0 = 0
    public int size() {
        return (tail - head) & (elements.length - 1);
    }

    // add the item to the front
    public void addFirst(Item item) {
        if (item == null) throw new IllegalArgumentException();
        elements[head = (head - 1) & (elements.length - 1)] = item;
        if (head == tail) doubleCapacity();
    }

    // add the item to the back
    public void addLast(Item item) {
        if (item == null) throw new IllegalArgumentException();
        elements[tail] = item;
        if ( (tail = (tail + 1) & (elements.length - 1)) == head) doubleCapacity();
    }

    // remove and return the item from the front
    public Item removeFirst() {
        int h = head;
        //@SuppressWarnings("unchecked")
        Item result = (Item) elements[h];
        if (result == null) throw new NoSuchElementException();
        elements[h] = null;
        head = (h + 1) & (elements.length - 1);
        return result;
    }

    // remove and return the item from the back
    public Item removeLast() {
        int t = (tail - 1) & (elements.length - 1);
        //@SuppressWarnings("unchecked")
        Item result = (Item) elements[t];
        if (result == null) throw new NoSuchElementException();
        elements[t] = null;
        tail = t;
        return result;
    }

    // return an iterator over items in order from front to back
    public Iterator<Item> iterator() {
        return new DeqIterator();
    }

    private class DeqIterator implements Iterator<Item> {
        private int cursor = head;

        private final int fence = tail;

        public boolean hasNext() {
            return cursor != fence;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException("MyDeque don't support remove from iterator");
        }

        public Item next() {
            if (cursor == fence) throw new NoSuchElementException();
            //@SuppressWarnings("unchecked")
            Item result = (Item) elements[cursor];
            cursor = (cursor + 1) & (elements.length - 1);
            return result;
        }
    }

    // unit testing (required)
    public static void main(String[] args) {
        Deque<String> deque = new Deque<>();
        deque.addFirst("HELLO");
        deque.addLast("WORLD");
        deque.addFirst("MARVIN");
        deque.removeLast();
        deque.removeFirst();
        deque.forEach(System.out::println);
    }

}