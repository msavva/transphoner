package org.babysherlock.util;

import java.util.Comparator;
import java.util.TreeSet;

/**
 * TreeSet that is bounded in size - keep maxSize highest items
 * NOTE: Uses set so cannot put in duplicate values...  comparator can't return 0
 *
 * @author Angel Chang
 */
public class BoundedTreeSet<E> extends TreeSet<E> {

    private int maxSize;

    public BoundedTreeSet(int maxSize) {
        super(new NaturalComparator());
        this.maxSize = maxSize;
    }

    public BoundedTreeSet(int maxSize, Comparator<E> comparator) {
        super(comparator);
        this.maxSize = maxSize;
    }


    public int getMaxSize() { return maxSize; }

    private static class NaturalComparator<T extends Comparable<? super T>> implements Comparator<T> {
        @Override
        public int compare(T o1, T o2) {
            return o1.compareTo(o2);
        }
    }

    /**
     * @return true if element was added, false otherwise
     * */
    @Override
    public boolean add(E e) {
        if (maxSize > 0 && size() < maxSize) {
            // queue isn't full => add element
            super.add(e);
            return true;
        } else {
            // there is already 1 or more elements => compare to the least
            int compared = super.comparator().compare(e, this.first());
            if (compared > 0) {
                // new element is larger than the least in queue => pull the least and add new one to queue
                pollFirst();
                super.add(e);
                return true;
            } else {
                // new element is less than the least in queue => return false
                return false;
            }
        }
    }
}