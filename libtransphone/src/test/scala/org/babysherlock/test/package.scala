package org.babysherlock

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers

/**
 * Test utilities
 * @author Angel Chang
 */
package object test extends FunSuite with BeforeAndAfter with ShouldMatchers {
  def checkEquals[T](message: String, actual: T, expected: T) {
    assert( actual === expected, message)
  }

  def checkSequenceSize[T](message: String, actual: Seq[T], expected: Seq[T], check: (String,T,T) => Unit = null) {
    if (expected == null || expected.isEmpty) {
      assert( actual == null || actual.isEmpty, message + " should be empty")
    } else {
      assert( actual != null && !actual.isEmpty,  message + " should be nonempty")
      assert( actual.length === expected.length, message + ".length" )
    }
  }

  def checkSequence[T,ET](message: String, actual: Seq[T], expected: Seq[ET], check: (String,T,ET) => Unit = null) {
    if (expected == null || expected.isEmpty) {
      assert( actual == null || actual.isEmpty, message + " should be empty")
    } else {
      assert( actual != null && !actual.isEmpty,  message + " should be nonempty")
      assert( actual.length === expected.length, message + ".length" )
      for (i <- 0 until actual.length) {
        if (check != null) {
          check(message + "." + i, actual(i), expected(i))
        } else {
          checkEquals(message + "." + i, actual(i), expected(i))
        }
      }
    }
  }

  def checkDouble(tolerance: Double)(message: String, actual: Double, expected: Double) {
    withClue(message)(actual should be (expected plusOrMinus tolerance))
  }

  def checkScored[T](tolerance: Double)(message: String, actual: (T,Double), expected: (T,Double)) {
    assert( actual._1 == expected._1, message)
    withClue(message)(actual._2 should be (expected._2 plusOrMinus tolerance))
  }
}
