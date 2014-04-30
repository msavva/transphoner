package org.babysherlock.transphoner.phonetics

import org.babysherlock.transphoner._
import org.babysherlock.transphoner.dict.Word
import org.babysherlock.transphoner.phonetics.PhoneSimilarity.AlineSimilarity
import org.babysherlock.util.{IOUtils, CSVFile}
import java.io.PrintWriter

/**
 * Similarity functions on Phones.
 */
object PhoneSimilarity {
  // Default exact match (0 / 1) similarity
  val ExactMatchPhoneSimilarity = new ExactMatchSimilarity[Phone]
  val ExactThenNonDiacriticPhoneSimilarity = new ExactThenNonDiacriticPhoneSimilarity
  val DistMatrixPhonePattern: Map[(Phone,Phone),Double] = Map()
  val ASJP_DIST = readDistMatrix(Constants.ASJP_DIST_MATRIX_VOWEL) ++ readDistMatrix(Constants.ASJP_DIST_MATRIX_CONS)
  val MIELKE_IPA_DIST = readDistMatrix(Constants.MIELKE_IPA_ACOUSTIC_DIST_MATRIX, normalize = true)
  val MielkeASJPPhonePatternSimilarity = new ASJPPhoneSimilarity(ASJP_DIST, 2*ASJP_DIST.values.max)
  val MielkePhonePatternSimilarity = new IPAPhoneSimilarity(MIELKE_IPA_DIST, 0.5*MIELKE_IPA_DIST.values.max, 2*MIELKE_IPA_DIST.values.max)
  val ASJPPhonePatternSimilarity = new ASJPPhoneSimilarity(Map())
  val BasicPhoneCategorySimilarity = new PhoneCategorySimilarity
  val SimplifiedCovingtonSimilarity = new SimplifiedCovingtonSimilarity
  val AlineSimilarity = new SimilarityWithCache(new AlineSimilarity)
  val AlineExactSimilarity = new SimilarityWithCache(new InterpolatedSimilarity[Phone](
    Seq( (AlineSimilarity, 1), (new SimilarityWithNull(ExactThenNonDiacriticPhoneSimilarity), 1) )
  ))
  //  val AlineSimilarity = new AlineSimilarity

  private val phoneSimilarities: Map[String, Similarity[Phone]] = Map(
    "Exact" -> ExactMatchPhoneSimilarity,
    "Mielke" -> MielkePhonePatternSimilarity,
    "MielkeASJP" -> MielkePhonePatternSimilarity,
    "ASJP" -> ASJPPhonePatternSimilarity,
    "BasicCategory" -> BasicPhoneCategorySimilarity,
    "SimplifiedCovington" -> SimplifiedCovingtonSimilarity,
    "AlineSimilarity" -> AlineSimilarity,
    "AlineExactSimilarity" -> AlineExactSimilarity
  )
  val similarities = phoneSimilarities.keys
  def apply(s: String) = phoneSimilarities.getOrElse(s, null)


  def printPhoneSimilarities(outputWriter: PrintWriter, phoneSim: Similarity[Phone]) {
    val phones = IPA.phones
    outputWriter.println("," + phones.mkString(","))
    for (p1 <- phones) {
      outputWriter.println( p1 + "," + phones.map( p2 => phoneSim.apply(p1,p2) ).mkString(",") )
    }
  }

  def printPhoneSimilarities(outputFilename: String, phoneSim: Similarity[Phone]) {
    val writer = IOUtils.filePrintWriter(outputFilename)
    printPhoneSimilarities(writer, phoneSim)
    writer.close
  }

  class ExactThenNonDiacriticPhoneSimilarity extends Similarity[Phone] {
    def distance(t1: Phone, t2: Phone): Double =
      if (t1.equals(t2)) 0
      else if (t1.getBasePhone.equals(t2.getBasePhone)) 0.1
      else 1
    override def maxDistance: Double = 1
  }

  // Phone-Phone Similarity through ASJP codes and experimental distance table
  class ASJPPhoneSimilarity(map: Map[(String,String),Double], defaultCost: Double = 1) extends Similarity[Phone] {
    val baseSimilarity = new MapBasedSimilarityWithReverseLookup[String](map, defaultCost)
    def distance(p1: Phone, p2: Phone): Double = {
      if (p1.equals(p2)) 0 else {
        baseSimilarity(p1.asjp, p2.asjp) + 0.1
      }
    }
    override def maxDistance: Double = math.max(baseSimilarity.maxDistance + 0.1, defaultCost)
  }

  class IPAPhoneSimilarity(map: Map[(String,String),Double], consVowCost: Double = 0, defaultCost: Double = 1) extends Similarity[Phone] {
    val baseSimilarity = new MapBasedSimilarityWithReverseLookup[String](map, defaultCost)
    def distance(p1: Phone, p2: Phone): Double = {
      val extraCost = if (p1.isVowel != p2.isVowel || p1.isConsonant != p2.isConsonant) consVowCost else 0
      baseSimilarity(p1.getBasePhone.ipa, p2.getBasePhone.ipa) + extraCost
    }
    override def maxDistance: Double = math.max(baseSimilarity.maxDistance, defaultCost)
  }

  class PhoneCategorySimilarity(sameCategoryCost: Double = 0.2) extends Similarity[Phone] {
    def distance(p1: Phone, p2: Phone): Double = {
      if (p1.equals(p2)) 0.0 else {
        if (p1.category == p2.category) sameCategoryCost else 1.0
      }
    }
    override def maxDistance: Double = 1.0
  }

  // Simplified version of Covington's distance function
  // http://aclweb.org/anthology/A/A00/A00-2038.pdf
  class SimplifiedCovingtonSimilarity() extends Similarity[Phone] {
    def distance(p1: Phone, p2: Phone): Double = distance_(p1,p2)/100

    private def distance_(p1: Phone, p2: Phone): Double = {
      if (p1 == null || p2 == null) 50.0
      else if (p1.equals(p2)) {
        if (p1.isConsonant) {
          0.0
        } else if (p1.isVowel) {
          5.0
        } else {
          // something else
          0.0
        }
      } else {
        if (p1.isVowel && p2.isVowel) {
          30.0
        } else if (p1.isConsonant && p2.isConsonant) {
          60.0
        } else {
          100.0
        }
      }
    }
    override def maxDistance: Double = 1.0
    override def supportsNull: Boolean = true
  }

  // Aline similarity
  // http://aclweb.org/anthology/A/A00/A00-2038.pdf
  class AlineSimilarity() extends Similarity[Phone] {
    def distance(p1: Phone, p2: Phone): Double = {
      -similarity(p1,p2)
    }
    override def similarity(p1: Phone, p2: Phone): Double = {
      rawSimilarity(p1,p2)/100
    }

    private def rawSimilarity(p1: Phone, p2: Phone): Double = {
      // sigma_skip(p) = Cskip
      if (p1 == null || p2 == null) AlinePhoneFeature.Cskip
      else if (p1.isSyllableSep || p2.isSyllableSep) {
        if (p1 == p2) AlinePhoneFeature.Csepmatch
        else AlinePhoneFeature.Csepmismatch
      }
      // sigma_sub(p,q) = Csub - delta(p,q) - V(p) - V(q)
      else AlinePhoneFeature.Csub - delta(p1,p2) - v(p1) - v(p2)
      // Not included sigma exp
      // sigma_exp(p,q1q2) = Cexp - delta(p, ql)- delta(p, q2) - V(p) - max(V(ql), V(q2))
    }

    def delta(p1: Phone, p2: Phone): Double = {
      val r = if (p1.isConsonant || p2.isConsonant) AlinePhoneFeature.CONSONANT_FEATURES else AlinePhoneFeature.VOWEL_FEATURES
      val f1 = p1.alineFeatures
      val f2 = p2.alineFeatures
      var sum: Double = 0
      for (f <- r) {
        var d = math.abs(f1(f.id) - f2(f.id))
        // Handle phonemes with double places of articulation
        if (f == AlinePhoneFeature.Place) {
          if (p1.place2 != null) {
            // Check place2 of p1 with p2's place1
            d = math.min(d, math.abs(f1(AlinePhoneFeature.Place2.id) - f2(f.id)))
          }
          if (p2.place2 != null) {
            // Check place2 of p2 with p1's place1
            d = math.min(d, math.abs(f2(AlinePhoneFeature.Place2.id) - f1(f.id)))
          }
          if (p1.place2 != null && p2.place2 != null) {
            // Check place2 of p1 with p2's place2
            d = math.min(d, math.abs(f1(AlinePhoneFeature.Place2.id) - f2(AlinePhoneFeature.Place2.id)))
          }
        }
        sum = sum + d*AlinePhoneFeature.weights(f.id)
      }
      sum
    }
    def v(p: Phone): Double = {
      if (p.isVowel) AlinePhoneFeature.Cvwl else 0.0
    }
    override def supportsNull: Boolean = true
  }

  // Levenshtein based Word similarity using Phone similarities
  class LevenshteinWordPhoneSimilarity(phoneSim: Similarity[Phone]) extends Similarity[Word] {
    val levSim = WeightedLevenshteinSimilarity[Phone](phoneSim)
    def distance(w1: Word, w2: Word): Double = {
      levSim(w1.phones, w2.phones)
    }
    def distance(w1: Seq[Phone], w2: Seq[Phone]): Double = {
      levSim(w1, w2)
    }
  }

  def mostDifferent(phoneSeqSim: Similarity[Seq[Phone]], phoneSeqs: Iterable[ Seq[Phone] ]): (Seq[Phone], Seq[Phone], Double)  = {
    val sims = for (p1 <- phoneSeqs; p2 <- phoneSeqs ) yield {
      (p1, p2, phoneSeqSim(p1,p2) )
    }
    sims.maxBy( _._3 )
  }

  def mostDifferent(phoneSeqSim: Similarity[Seq[Phone]], word: Word): (Seq[Phone], Seq[Phone], Double) = {
    mostDifferent(phoneSeqSim, word.pronunciations)
  }

  def readDistMatrix(file: String, normalize: Boolean = false): Map[(String,String),Double] = {
    val csvFile = new CSVFile(file, includesHeader = true)
    val map = csvFile.toPairMap.mapValues(s => s.toDouble)
    if (normalize) {
      val max = map.values.max
      if (max > 0) map.mapValues(s => s / max) else map
    } else map
  }
}

class AlineAlignerSimilarity[T <: Phone] extends Similarity[Seq[T]] {
  val sim = new AlineSimilarity

  def distance(x: Seq[T], y: Seq[T]): Double = {
    val m = x.length + 1
    val n = y.length + 1

    // Similarity matrix initialization
    val S = Array.ofDim[Double](m, n)
    for (i <- 0 until m) S(i)(0) = 0
    for (j <- 0 until n) S(0)(j) = 0

    // Dynamic programming to build similarity matrix
    for (i <- 1 until m; j <- 1 until n) {
      val xi = x(i-1)
      val yj = y(j-1)
      val ss = Array[Double](
        S(i-1)(j)   + sim.similarity(xi, null),
        S(i)(j-1)   + sim.similarity(null,yj),
        S(i-1)(j-1) + sim.similarity(xi,yj)
        //TODO: Implement expansion operations
        //,0
      )
      S(i)(j) = ss.max
    }

    // Return negative of end similarity as minimum distance
    -S(m-1)(n-1)
  }
}


