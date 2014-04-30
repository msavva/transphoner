package org.babysherlock.transphoner

import junit.framework.TestCase

/**
 * Test Transphoner similarity
 * @author Angel Chang
 */
class SimilarityTest extends TestCase {
  def testWeightedLevenshteinSimilarityExact() {
    val sim = new WeightedLevenshteinSimilarity[Char]()
    assert(0 == sim.distance("abc", "abc"))
    assert(1 == sim.distance("abd", "abc"))
    assert(1 == sim.distance("abd", "abcd"))
    assert(2 == sim.distance("abde", "abcd"))
  }

  def testWeightedLevenshteinSimilarity() {
    val mysim = new MapBasedSimilarity[Char](
      Map(('c','d') -> 0.3, ('d','c') -> 0.5)
    )
    val sim = WeightedLevenshteinSimilarity[Char](mysim)
    assert(0 == sim.distance("abc", "abc"))
    assert(0.5 == sim.distance("abd", "abc"))
    assert(0.3 == sim.distance("abc", "abd"))
    assert(1 == sim.distance("abd", "abcd"))
    assert(1.5 == sim.distance("abde", "abcd"))
  }
}
