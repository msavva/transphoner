package org.babysherlock.transphoner.app

import org.babysherlock.nlp.UWN
import org.babysherlock.transphoner.dict.{Language, Word}
import org.babysherlock.transphoner.{Similarity, TransPhoner, TransPhonerOptions}
import org.babysherlock.transphoner.semantics.SemanticSimilarity
import org.babysherlock.util.BatchSampler
import scala.util.Random

/**
 * Wrapper around transphoner and alternative methods for suggesting keywords
 * @author Angel Chang
 */
object KeywordSuggester {
  type SuggesterFn = (Seq[Word],Int) => Seq[Keywords]

  def uniform(srcLangId: String, tgtLangId: String): KeywordSuggester =
    uniform(Language(srcLangId), Language(tgtLangId))
  def uniform(srcLang: Language, tgtLang: Language): KeywordSuggester =
    new KeywordSuggester(srcLang, tgtLang, uniformRandomSuggester(tgtLang.words.toIndexedSeq)_)
  def uniformMostFrequent(srcLangId: String, tgtLangId: String, n: Int): KeywordSuggester =
    uniformMostFrequent(Language(srcLangId), Language(tgtLangId), n)
  def uniformMostFrequent(srcLang: Language, tgtLang: Language, n: Int): KeywordSuggester =
    new KeywordSuggester(srcLang, tgtLang, uniformRandomSuggester(tgtLang.wordsMostFrequent.take(n))_)

  def similarUWNPath(srcLangId: String, tgtLangId: String): KeywordSuggester =
    similarUWNPath(Language(srcLangId), Language(tgtLangId))
  def similarUWNPath(srcLang: Language, tgtLang: Language): KeywordSuggester =
    new KeywordSuggester(srcLang, tgtLang, similarUWNPathSuggester(tgtLang)_)
  def similarSemantics(srcLang: Language, tgtLang: Language, sim: SemanticSimilarity): KeywordSuggester =
    new KeywordSuggester(srcLang, tgtLang, semanticSimilaritySuggester(tgtLang.words, sim)_)
  def similarWords(srcLang: Language, tgtLang: Language, sim: Similarity[Word]): KeywordSuggester =
    new KeywordSuggester(srcLang, tgtLang, wordSimilaritySuggester(tgtLang.words, sim)_)

  def transphoner(srcLangId: String, tgtLangId: String): KeywordSuggester = transphoner(Language(srcLangId), Language(tgtLangId))
  def transphoner(srcLang: Language, tgtLang: Language): KeywordSuggester =
    new KeywordSuggester(srcLang, tgtLang, transphonerSuggester(
      TransPhonerOptions(srcLang, tgtLang)
    )_)
  def transphoner(options: TransPhonerOptions): KeywordSuggester =
    new KeywordSuggester(options.source, options.target, transphonerSuggester(
      options
    )_)
  def transphoner(srcLangId: String, tgtLangId: String, options: TransPhonerOptions): KeywordSuggester =
    transphoner(Language(srcLangId), Language(tgtLangId), options)
  def transphoner(srcLang: Language, tgtLang: Language, options: TransPhonerOptions): KeywordSuggester =
    new KeywordSuggester(options.source, options.target, transphonerSuggester(
      options.copy(srcLang, tgtLang)
    )_)

  def uniformRandomSuggester(targetWords: IndexedSeq[Word])(phrase: Seq[Word], n: Int) = {
    val sampler = new BatchSampler
    sampler.sampleWithReplacement(targetWords,n).map( s => Keywords(Seq(s), 0.0)).toSeq
  }

  private def selectRandom[T](seq: Seq[Seq[T]], n: Int, rand: Random = Random): Seq[Seq[T]] = {
    for (i <- 0 until n) yield {
      seq.map( x => {
        val r = rand.nextInt(x.size)
        x(r)
      })
    }
  }


  def similarUWNPathSuggester(tgtLang: Language, steps: Int = 3)(phrase: Seq[Word], n: Int) = {
    val randomTerms = phrase.map( w => UWN.randomRelatedTerms(
      w.orthographies.head, w.lang.uwnCode, tgtLang.uwnCode, steps, n+5).toIndexedSeq )
    val randomWords = randomTerms.map( x => x.map( y => tgtLang.lookup(y) ).filter ( y => y != null) ).filter( x => x.size > 0 )
    selectRandom(randomWords, n).map( w => Keywords(w, 0.0) )
  }

  def wordSimilaritySuggester(candidates: Set[Word], sim: Similarity[Word], nbest: Int = 10, threshold: Int = 0)(phrase: Seq[Word], n: Int) = {
    val c = math.max(nbest, n)
    val randomWords = phrase.map( w =>  { sim.mostSimilar(w, candidates, c, useSim = true).filter( p => p._2 > threshold) } )
    selectRandom(randomWords, n).map( w => Keywords(w.map(x => x._1), w.map(x => x._2).sum) )
  }

  def semanticSimilaritySuggester(candidates: Set[Word], sim: SemanticSimilarity, nbest: Int = 10, threshold: Int = 0)(phrase: Seq[Word], n: Int) = {
    val c = math.max(nbest, n)
    val randomWords = phrase.map( w =>  { sim.mostSimilar(w, candidates, c, useSim = true).filter( p => p._2 > threshold) } )
    selectRandom(randomWords, n).map( w => Keywords(w.map(x => x._1), w.map(x => x._2).sum) )
  }

  def transphonerSuggester(transphonerOptions: TransPhonerOptions)(phrase: Seq[Word], n: Int): Seq[Keywords] =
    transphonerSuggester(TransPhoner(transphonerOptions))(phrase, n)
  def transphonerSuggester(transphoner: TransPhoner)(phrase: Seq[Word], n: Int): Seq[Keywords] = {
    transphoner.transphone(phrase, n).map( m => Keywords(m.target, m.score) )
  }

}

class KeywordSuggester(val srcLang: Language,
                       val tgtLang: Language,
                       val suggester: KeywordSuggester.SuggesterFn) {
  def this(src: String, tgt: String, suggester: KeywordSuggester.SuggesterFn) =
    this(Language(src), Language(tgt), suggester)

  def suggestKeywords(phrase: String, n: Int): Seq[Keywords] = {
    val words = srcLang.toWords(phrase)
    suggestKeywords( words, n)
  }
  def suggestKeywords(word: Word, n: Int): Seq[Keywords] = suggestKeywords(Seq(word), n)
  def suggestKeywords(words: Seq[Word], n: Int): Seq[Keywords] = {
    val filtered = words.filter( w => w != null)
    if (filtered.nonEmpty) suggester(filtered, n) else Seq()
  }
}

case class Keywords(words: Seq[Word], score: Double)



