package org.babysherlock.util

import scala.collection.mutable

/**
 * Priority queue that is bounded in size
 *
 * @author Angel Chang
 */
class BoundedPriorityQueue[T](val maxSize: Int)(implicit ord : scala.Ordering[T]) extends mutable.PriorityQueue[T] {
  override def +=(elem : T) : BoundedPriorityQueue.this.type = {
    if (this.size >= maxSize) {
      // keep maxSize lowest priorities...
      // compare against element with highest priority, if we are lower add ourselves
      if (ord.compare(elem,this.head) < 0) {
        this.dequeue
        super.+=(elem)
      } else this
    } else {
      super.+=(elem)
    }
  }
}
