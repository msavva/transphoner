package org.babysherlock.transphoner

import org.babysherlock.transphoner.dict.{WordConverter, Word}
import org.babysherlock.transphoner.phonetics.Phone
import edu.stanford.nlp.tokensregex.matcher.{Match, TrieMap}
import scala.collection.JavaConversions._

/**
 * Helper class for working with the TrieMap in JavaNLP
 * @author Angel Chang
 */
object TrieHelper {
  def toPhoneTrie(words: Iterable[Word], wordConverter: WordConverter ): TrieMap[Phone, Set[Word]] = {
    val targetTrie = new TrieMap[Phone,Set[Word]]()
    for (w <- words) {
      val phones = wordConverter.toPhonesToMatch(w)
      val ws = trieGet(targetTrie, phones)
      if (ws != null) {
        targetTrie.put( phones , ws + w)
      } else {
        targetTrie.put( phones, Set(w))
      }
    }
    targetTrie
  }
  def toCharTrie(words: Iterable[Word] ): TrieMap[Char, Set[Word]] = {
    val targetTrie = new TrieMap[Char,Set[Word]]()
    for (w <- words; str <- w.orthographies ) {
      val chars: Seq[Char] = str.toSeq
      val ws = trieGet(targetTrie, chars )
      if (ws != null) {
        targetTrie.put( chars , ws + w )
      } else {
        targetTrie.put( chars, Set(w) )
      }
    }
    targetTrie
  }
  def pairsToTrie[K,V](pairs: Iterable[(Iterable[K],V)] ): TrieMap[K, Set[V]] = {
    val targetTrie = new TrieMap[K,Set[V]]()
    for (p <- pairs) {
      val ks: Seq[K] = p._1.toSeq
      val ws = trieGet(targetTrie, ks )
      if (ws != null) {
        targetTrie.put( ks, ws + p._2 )
      } else {
        targetTrie.put( ks, Set(p._2) )
      }
    }
    targetTrie
  }
  def pairsToCharTrie[V](pairs: Iterable[(String,V)] ): TrieMap[Char, Set[V]] = {
    pairsToTrie( pairs.map( p => ( p._1.toSeq, p._2 ) ))
  }
  def trieGet[K,V](trie: TrieMap[K,V], key: Iterable[K]) = {
    trie.get(scala.collection.JavaConversions.asJavaIterable(key))
  }
  def getAll[T](sets: Seq[Set[T]]): Seq[Seq[T]] = {
    if (sets.isEmpty) Seq()
    else {
      val first = sets.head.map( w => Seq(w) ).toSeq
      val rest = getAll(sets.tail)
      var all = Seq[Seq[T]]()
      if (rest.isEmpty) {
        all = first
      } else {
        for (s <- first; t <- rest) {
          all = all ++ Seq(s ++ t)
        }
      }
      all
    }
  }
  def toMatchScorer[K,V](scorer: Match[K,V] => Double): edu.stanford.nlp.util.Function[Match[K,V], java.lang.Double] = {
    val func = new edu.stanford.nlp.util.Function[Match[K,V], java.lang.Double] {
      override def apply(in: Match[K,V]): java.lang.Double = {
        scorer(in)
      }
    }
    func
  }
}


