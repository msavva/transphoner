package org.babysherlock.transphoner.dict

import org.babysherlock.transphoner.{SimilarityWithOption, Similarity}
import org.babysherlock.transphoner.phonetics._
import org.babysherlock.util.EnrichedCollections

/**
 * Represents a word in a given language.
 * @author Manolis Savva
 */
case class Word(nativeWord: String,
                lang: Language,
                // Full ipas for this word (includes diacritics and everything
                fullIpas: Seq[String]) {
  def orthographies: Set[String] = Set(nativeWord)

  def gloss: String = nativeWord
  lazy val phones: Seq[Phone] = {
    val p = IPA.strToPhoneSeq(fullIpas.head)
    // Drop initial glottal stop
    if (p.nonEmpty && p.head.isGlottalStop) p.tail else p
  }
  lazy val pronunciations: Seq[Seq[Phone]] = fullIpas.map ( s => IPA.strToPhoneSeq(s) )
  // Reduced ipa for marks we actually care about
  def ipa: String = phones.mkString
  def ipas: Seq[String] = pronunciations.map( p => p.mkString )

  // Stored with the language (word frequency, count, rank)
  lazy val freq: Double = if (lang != null) lang.wordFreq(this) else 0
  lazy val count: Double = if (lang != null) lang.wordCount(this) else 0
  lazy val rank: Int = if (lang != null) lang.wordRank(this) else 0
  def mostDifferentPron(phoneSeqSim: Similarity[Seq[Phone]]): (Seq[Phone], Seq[Phone], Double) =
    PhoneSimilarity.mostDifferent(phoneSeqSim, this)
  override def toString: String = nativeWord + ":" + ipa
  override def hashCode = 41*(41 + lang.hashCode) + nativeWord.hashCode
  override def equals(other: Any) = other match {
    case that: Word => (that canEqual this) &&
      (this.nativeWord == that.nativeWord) && (this.lang == that.lang)
    case _ => false
  }
  def canEqual(other: Any) = other.isInstanceOf[Word]

}

class DictWord(nativeWord: String,
               lang: Language,
               // all pronunciations
               fullIpas: Seq[String],
               // all ways of writing the word
               override val orthographies: Set[String],
               // word definition
               override val gloss: String) extends Word(nativeWord, lang, fullIpas)
{
}

class IPAWord(ipa: String) extends Word(ipa, IPALanguage, Seq(ipa)) {
  override lazy val freq = 0.0
  override lazy val count = 0.0
  override lazy val rank = 0
}

object NullWord extends Word("", null, Seq("∅")) {
  override lazy val phones = Seq(PhoneNull)
}

// Word that is a result of transphoning (not used yet)
//class TransphonedWord(val baseWord: Word,
//                      val syllables: Seq[Syllable],
//                      val isStressed: Boolean = false)
//  extends Word(baseWord.nativeWord, baseWord.lang, baseWord.fullIpas)
//{
//  override def orthographies = baseWord.orthographies
//  override def gloss = baseWord.gloss
//}

case class Syllable(phones: Seq[Phone],
                    baseStressType: StressType.Value,
                    tones: Seq[Tone] = Seq()) {
  override def toString: String = {
    StressType.toIPA(baseStressType) + phones.mkString
  }
  lazy val stressType =
    if (baseStressType != null && baseStressType != StressType.none) baseStressType
    else if (tones.exists( s => s.stressType == StressType.primary )) StressType.primary
    else if (tones.exists( s => s.stressType == StressType.secondary )) StressType.secondary
    else StressType.none
}

object Syllable {
  val SyllableStressMatchSimilarity = new Similarity[Syllable]() {
    override def distance(t1: Syllable, t2: Syllable): Double = if (t1.stressType == t2.stressType) 0 else 1
    override def maxDistance: Double = 1
    override def supportsNull: Boolean = false
  }
  val SyllableStressMatchSimilarityWithOption = new SimilarityWithOption[Syllable](SyllableStressMatchSimilarity)
}

class WordConverter(val syllabify: Boolean = false,
                    val ignoreTones: Boolean = false) {
  /**
   * Takes a sequence of words and convert it into a sequence of phones
   *   If syllabify is true, then an syllable break is inserted between all words
   * @param words sequence of words
   * @return sequence of phones
   */
  def toPhonesToMatch(words: Seq[Word]): Seq[Phone] = {
    val phoneSeqs = words.map(w => toPhonesToMatch(w))
    // Add syllable separators between words if syllabify
    if (syllabify) phoneSeqs.reduce( (a,b) => if (a.isEmpty) b else if (b.isEmpty) a else a ++ Seq(IPA.syllableSep) ++ b )
    else phoneSeqs.flatten
  }

  def toPhonesToMatch(word: Word): Seq[Phone] = {
    var phones = word.phones.filter( phone => !phone.isStressMarker )
    if (!syllabify) phones = phones.filter( phone => !phone.isSyllableSep )
    if (ignoreTones) phones = phonesWithoutTones(phones)
    phones
  }

  def phonesWithoutTones(phones: Seq[Phone]): Seq[Phone] = {
    phones.filter( phone => !phone.isTone ).map( phone => if (phone.hasTone) phone.getBasePhone else phone )
  }

  // Returns all phones in word - adds syllable phones in between
  def toPhones(words: Seq[Word]): Seq[Phone] = {
    val phoneSeqs = words.map(w => w.phones)
    // Add syllable separators between words
    phoneSeqs.reduce( (a,b) => if (a.isEmpty) b else if (b.isEmpty) a else a ++ Seq(IPA.syllableSep) ++ b )
  }

  // Returns phones broken into syllables, stress type is indicated
  def toSyllables(word: Word): Seq[Syllable] = {
    val syllables = EnrichedCollections.split[Phone](word.phones, p => p.isSyllableSep)
    syllables.map( seq => {
      val phones = seq //if (ignoreTones) phonesWithoutTones() else seq
      val stressType = phonesToStressType(seq)
      val tones = phones.map( p => word.lang.lookupTone( p.tones.mkString ) ).filter( t => t != null )
      Syllable( phones.filter( p => !p.isStressMarker ),  stressType, tones)
    })
  }

  def toSyllables(words: Seq[Word]): Seq[Seq[Syllable]] = {
    words.map( w => toSyllables(w) )
  }

  def phonesToStressType(seq: Seq[Phone]): StressType.Value = {
    if (seq.exists( p => p.isPrimaryStress )) StressType.primary
    else if (seq.exists( p => p.isSecondaryStress )) StressType.secondary
    else StressType.none
  }

  def syllablesToStressType(syllables: Seq[Syllable]): StressType.Value = {
    if (syllables.exists( s => s.stressType == StressType.primary )) StressType.primary
    else if (syllables.exists( s => s.stressType == StressType.secondary )) StressType.secondary
    else StressType.none
  }
}

// Part of speech
object Pos extends Enumeration {
  type Pos = Value
  val Noun, Verb, Adjective, Adverb, Interjection, Pronoun, Preposition, Conjunction, Article,Determiner, Number, Not, To, Ex = Value

  def fromString(s: String): Value = {
    Pos.withName(s)
  }
}

object Gender extends Enumeration {
  type Gender = Value
  val FEM = Value("f")
  val MASC = Value("m")
  val NEUT = Value("n")
}

object Number extends Enumeration {
  type Number = Value
  val SINGLE = Value("s")
  val PLURAL = Value("pl")
}

