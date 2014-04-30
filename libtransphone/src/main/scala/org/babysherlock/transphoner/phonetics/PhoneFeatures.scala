package org.babysherlock.transphoner.phonetics

import org.babysherlock.util.{CSVFile, IOUtils}
import org.babysherlock.transphoner.Constants

/**
 * Phone Features
 * @author Angel Chang
 */
object PhoneFeatures extends App {
  def readFeaturesFile(filename: String): Map[String, Array[PhoneFeatureValue.Value]] = {
    val csvReader = new CSVFile(filename, includesHeader = true)
    // Check header
    val header: Array[String] = csvReader.getHeader()
    val expected = Seq("IPA") ++ PhoneFeature.values.toSeq.map(v => v.toString)
    if (header.size != expected.size) throw new RuntimeException(
      "Feature file header length " + header.size + " does not match expected" + expected.size)
    for (i <- 0 until header.size)  {
      if (header(i) != expected(i)) throw new RuntimeException(
        "Feature file header " + i + "(" + header(i) + ") does not match expected (" + expected(i) + ")")
    }
    // Convert rows to feature array
    csvReader.map( row => row(0) -> row.drop(1).map( f => PhoneFeatureValue.withName(f) ) ).toMap
  }

  /**
   * Takes our phone features and creates a feature file
   * @param filename Name of the feature file
   */
  def makeFeaturesFile(filename: String, phones: Seq[Phone], phoneFeatures: Map[String, Array[PhoneFeatureValue.Value]] = Map()) {
    val pw = IOUtils.filePrintWriter(filename)
    pw.println("IPA," + PhoneFeature.values.mkString(","))
    for (phone <- phones) {
      val features = phoneFeatures.getOrElse(phone.ipa, PhoneFeature.features(phone))
      pw.println(phone.ipa + "," + features.mkString(","))
    }
    pw.close()
  }

  makeFeaturesFile(Constants.TRANSPHONER_DATA_DIR + "ipaFeatures.csv", IPA.phones)
}

object AlinePhoneFeature extends Enumeration {
  type AlinePhoneFeature = Value
  val Syllabic = Value("syllabic")
  val Voice = Value("voice")
  val Lateral = Value("lateral")
  val High = Value("high")      // multivalued
  val Manner = Value("manner")  // multivalued
  val Long = Value("long")
  val Place = Value("place")    // multivalued
  val Place2 = Value("place2")  // multivalued  (2nd place of articulation)
  val Nasal = Value("nasal")
  val Aspirated = Value("aspirated")
  val Back = Value("back")      // multivalued
  val Retroflex = Value("retroflex")
  val Round = Value("round")

  val VOWEL_FEATURES = Set(Syllabic, Nasal, Retroflex, High, Back, Round, Long)
  val CONSONANT_FEATURES = Set(Syllabic, Manner, Voice, Nasal, Retroflex, Lateral, Aspirated, Place)

  // Constant scores
  val Cskip = -10  // indels
  val Csub = 35    // substitutions
  val Cexp = 45    // expansions
  val Cvwl = 10    // relative weights of vowels and consonants

  // Score for matching syllable break (added)
  val Csepmatch = 20
  val Csepmismatch = Cskip

  // Saliences
  val weights: Array[Double] = Array[Double](5, 10, 10, 5, 50, 1, 40, 40, 10, 5, 5, 10, 5)

  private def _placeValue(place: PhoneArticulationPlace.Value): Double = {
    place match {
      case PhoneArticulationPlace.Labial => 1.0
      case PhoneArticulationPlace.Labiodental => 0.95
      case PhoneArticulationPlace.Dental => 0.9
      case PhoneArticulationPlace.Alveolar => 0.85
      case PhoneArticulationPlace.Retroflex => 0.8
      case PhoneArticulationPlace.Postalveolar => 0.75 // Palato-alveolor?
      case PhoneArticulationPlace.Palatal => 0.7
      case PhoneArticulationPlace.Velar => 0.6
      case PhoneArticulationPlace.Uvular => 0.5
      case PhoneArticulationPlace.Pharyngeal => 0.3
      case PhoneArticulationPlace.Glottal => 0.1
      case _ => 0
    }
  }

  def features(phone: Phone): Array[Double] = {
    val f = Array.fill[Double](AlinePhoneFeature.maxId)(0.0)
    val basicFeatures = phone.basicFeatures
    f(AlinePhoneFeature.Syllabic.id) = if (basicFeatures(PhoneFeature.SYLL.id) == PhoneFeatureValue.POS) 1 else 0
    f(AlinePhoneFeature.Voice.id) = if (basicFeatures(PhoneFeature.VOICE.id) == PhoneFeatureValue.POS) 1 else 0
    f(AlinePhoneFeature.Lateral.id) = if (basicFeatures(PhoneFeature.LATERAL.id) == PhoneFeatureValue.POS) 1 else 0

    f(AlinePhoneFeature.High.id) =
      if (basicFeatures(PhoneFeature.HIGH.id) == PhoneFeatureValue.POS) 1.0
      else if (basicFeatures(PhoneFeature.LOW.id) == PhoneFeatureValue.POS) 0.0
      else 0.5

    f(AlinePhoneFeature.Place.id) = _placeValue(phone.place1)
    f(AlinePhoneFeature.Place2.id) = _placeValue(phone.place2)

    f(AlinePhoneFeature.Long.id) = if (basicFeatures(PhoneFeature.LONG.id) == PhoneFeatureValue.POS) 1 else 0

    f(AlinePhoneFeature.Manner.id) = phone.category match {
      case PhoneCategory.STOP | PhoneCategory.FLAP | PhoneCategory.PLOSIVE |
           PhoneCategory.IMPLOSIVE | PhoneCategory.CLICK => 1.0
      case PhoneCategory.AFFRICATE => 0.9
      case PhoneCategory.FRICATIVE => 0.8
      case PhoneCategory.APPROXIMANT | PhoneCategory.NASAL | PhoneCategory.TRILL => 0.6
      case PhoneCategory.VOWEL => f(AlinePhoneFeature.High.id) * 0.4
      case _ => 0
    }

    f(AlinePhoneFeature.Nasal.id) = if (basicFeatures(PhoneFeature.NASAL.id) == PhoneFeatureValue.POS) 1 else 0
    f(AlinePhoneFeature.Aspirated.id) = if (basicFeatures(PhoneFeature.ASPIRATED.id) == PhoneFeatureValue.POS) 1 else 0

    f(AlinePhoneFeature.Back.id) =
      if (basicFeatures(PhoneFeature.BACK.id) == PhoneFeatureValue.POS) 1.0
      else if (basicFeatures(PhoneFeature.FRONT.id) == PhoneFeatureValue.POS) 0.0
      else 0.5

    f(AlinePhoneFeature.Retroflex.id) = if (basicFeatures(PhoneFeature.RETROFLEX.id) == PhoneFeatureValue.POS) 1 else 0
    f(AlinePhoneFeature.Round.id) = if (basicFeatures(PhoneFeature.ROUND.id) == PhoneFeatureValue.POS) 1 else 0

    f
  }
}


// Phonetic features from https://dl.dropboxusercontent.com/u/5956329/Riggle/PhonChart_v1212.pdf
object PhoneFeature extends Enumeration {
  type PhoneFeature = Value
  // Class features
  val CONS = Value("cons")
  val SON = Value("son")
  val SYLL = Value("syll")

  // Place features
  val LABIAL = Value("labial")
  val ROUND = Value("round")

  val CORONAL = Value("coronal")
  val ANT = Value("ant")
  val DISTRIB = Value("distrib")

  val DORSAL = Value("dorsal")
  val HIGH = Value("high")
  val LOW = Value("low")
  val BACK = Value("back")
  val FRONT = Value("front")
  val TENSE = Value("tense")

  val PHRNGL = Value("phrngl")
  val ATR = Value("atr")

  // Laryngeal features
  val VOICE = Value("voice")
  val SG = Value("SG")
  val CG = Value("CG")

  // Manner features
  val CONT = Value("cont")
  val STRIDENT = Value("strident")
  val LATERAL = Value("lateral")
  val DELREL = Value("delrel")
  val NASAL = Value("nasal")

  // Other features
  val ASPIRATED = Value("aspirated")
  val LONG = Value("long")
  val RETROFLEX = Value("retroflex")



  /**
   * Takes our phones and creates features based on the description
   */
  def features(phone: Phone): Array[PhoneFeatureValue.Value] = {
    if (phone.isDiacritic || phone.isSuprasegmental) _diacriticFeatures(phone)
    else _normalFeatures(phone)
  }

  private def _diacriticFeatures(phone: Phone): Array[PhoneFeatureValue.Value] = {
    val desc = phone.description.toLowerCase
    val features = Array.fill[PhoneFeatureValue.Value](PhoneFeature.maxId)(PhoneFeatureValue.UNSPECIFIED)
    desc match {
      case "voiced" => features(PhoneFeature.VOICE.id) = PhoneFeatureValue.POS
      case "voiceless" => features(PhoneFeature.VOICE.id) = PhoneFeatureValue.NEG
      case "aspirated" => features(PhoneFeature.ASPIRATED.id) = PhoneFeatureValue.POS
      case "nasalization" => features(PhoneFeature.NASAL.id) = PhoneFeatureValue.POS
      case "long" => features(PhoneFeature.LONG.id) = PhoneFeatureValue.POS
      case "half long" => features(PhoneFeature.LONG.id) = PhoneFeatureValue.POS
      case "extra-short" => features(PhoneFeature.LONG.id) = PhoneFeatureValue.NEG
      case _ => {}
    }
    features
  }

  private def _normalFeatures(phone: Phone): Array[PhoneFeatureValue.Value] = {
    val desc = phone.description.toLowerCase
    val features = Array.fill[PhoneFeatureValue.Value](PhoneFeature.maxId)(PhoneFeatureValue.NEG)

    // Default for ant, distrib, atr to unspecified
    features(PhoneFeature.ANT.id) = PhoneFeatureValue.UNSPECIFIED
    features(PhoneFeature.DISTRIB.id) = PhoneFeatureValue.UNSPECIFIED
    features(PhoneFeature.ATR.id) = PhoneFeatureValue.UNSPECIFIED

    if (desc.contains("lateral")) features(PhoneFeature.LATERAL.id) = PhoneFeatureValue.POS
    if (phone.isConsonant) {
      features(PhoneFeature.CONS.id) = PhoneFeatureValue.POS
      features(PhoneFeature.ROUND.id) = PhoneFeatureValue.UNSPECIFIED
      if (desc.contains("voiced")) features(PhoneFeature.VOICE.id) = PhoneFeatureValue.POS
    }
    phone.place match {
      case PhoneArticulationPlace.Labial => {
        features(PhoneFeature.LABIAL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.ROUND.id) = PhoneFeatureValue.NEG
        features(PhoneFeature.HIGH.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.LOW.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.BACK.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.FRONT.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.TENSE.id) = PhoneFeatureValue.UNSPECIFIED
      }
      case PhoneArticulationPlace.Dental | PhoneArticulationPlace.Alveolar |
           PhoneArticulationPlace.Postalveolar => {
          features(PhoneFeature.CORONAL.id) = PhoneFeatureValue.POS
          if (phone.place == PhoneArticulationPlace.Postalveolar) {
            features(PhoneFeature.ANT.id) = PhoneFeatureValue.NEG
            features(PhoneFeature.DISTRIB.id) = PhoneFeatureValue.POS
          } else {
            features(PhoneFeature.ANT.id) = PhoneFeatureValue.POS
            features(PhoneFeature.DISTRIB.id) = PhoneFeatureValue.NEG
          }
          features(PhoneFeature.HIGH.id) = PhoneFeatureValue.UNSPECIFIED
          features(PhoneFeature.LOW.id) = PhoneFeatureValue.UNSPECIFIED
          features(PhoneFeature.BACK.id) = PhoneFeatureValue.UNSPECIFIED
          features(PhoneFeature.FRONT.id) = PhoneFeatureValue.UNSPECIFIED
          features(PhoneFeature.TENSE.id) = PhoneFeatureValue.UNSPECIFIED
      }
      case PhoneArticulationPlace.Retroflex => {
        features(PhoneFeature.RETROFLEX.id) = PhoneFeatureValue.POS
      }
      case PhoneArticulationPlace.Palatal => {
        features(PhoneFeature.DORSAL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.HIGH.id) = PhoneFeatureValue.POS
        if (phone.category == PhoneCategory.AFFRICATE ||
            phone.category == PhoneCategory.PLOSIVE ||
            phone.category == PhoneCategory.IMPLOSIVE ||
            phone.category == PhoneCategory.FRICATIVE) {
          features(PhoneFeature.ANT.id) = PhoneFeatureValue.NEG
          features(PhoneFeature.DISTRIB.id) = PhoneFeatureValue.POS
        }
      }
      case PhoneArticulationPlace.Velar => {
          features(PhoneFeature.DORSAL.id) = PhoneFeatureValue.POS
          features(PhoneFeature.HIGH.id) = PhoneFeatureValue.POS
          features(PhoneFeature.BACK.id) = PhoneFeatureValue.POS
      }
      case PhoneArticulationPlace.Uvular => {
          features(PhoneFeature.DORSAL.id) = PhoneFeatureValue.POS
          features(PhoneFeature.BACK.id) = PhoneFeatureValue.POS
      }
      case PhoneArticulationPlace.Pharyngeal => {
        features(PhoneFeature.PHRNGL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.ATR.id) = PhoneFeatureValue.NEG

        features(PhoneFeature.HIGH.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.LOW.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.BACK.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.FRONT.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.TENSE.id) = PhoneFeatureValue.UNSPECIFIED
      }
      case PhoneArticulationPlace.Glottal => {
        features(PhoneFeature.CONS.id) = PhoneFeatureValue.NEG

        features(PhoneFeature.HIGH.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.LOW.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.BACK.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.FRONT.id) = PhoneFeatureValue.UNSPECIFIED
        features(PhoneFeature.TENSE.id) = PhoneFeatureValue.UNSPECIFIED
      }
      case _ => {}
    }
    phone.category match {
      case PhoneCategory.FRICATIVE => {
        features(PhoneFeature.CONT.id) = PhoneFeatureValue.POS
      }
      case PhoneCategory.AFFRICATE => {
        features(PhoneFeature.CONT.id) = PhoneFeatureValue.POSNEG
        features(PhoneFeature.DELREL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.STRIDENT.id) = PhoneFeatureValue.POS
      }
      case PhoneCategory.NASAL => {
        features(PhoneFeature.NASAL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.VOICE.id) = PhoneFeatureValue.POS
        features(PhoneFeature.SON.id) = PhoneFeatureValue.POS
      }
      case PhoneCategory.APPROXIMANT | PhoneCategory.TRILL => {
        features(PhoneFeature.SON.id) = PhoneFeatureValue.POS
        features(PhoneFeature.VOICE.id) = PhoneFeatureValue.POS
        features(PhoneFeature.CONT.id) = PhoneFeatureValue.POS
      }
      case PhoneCategory.VOWEL => {
        features(PhoneFeature.SON.id) = PhoneFeatureValue.POS
        features(PhoneFeature.SYLL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.LABIAL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.VOICE.id) = PhoneFeatureValue.POS
        features(PhoneFeature.DORSAL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.PHRNGL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.CONT.id) = PhoneFeatureValue.POS
        if (!desc.contains("unrounded")) features(PhoneFeature.ROUND.id) = PhoneFeatureValue.POS
        if (desc.contains("mid")) {
          features(PhoneFeature.HIGH.id) = PhoneFeatureValue.NEG
          features(PhoneFeature.LOW.id) = PhoneFeatureValue.NEG
          if (desc.contains("close")) features(PhoneFeature.TENSE.id) = PhoneFeatureValue.POS
        } else if (desc.contains("close")) {
          features(PhoneFeature.HIGH.id) = PhoneFeatureValue.POS
          features(PhoneFeature.LOW.id) = PhoneFeatureValue.NEG
          if (!desc.contains("near-close")) features(PhoneFeature.TENSE.id) = PhoneFeatureValue.POS
        } else {
          features(PhoneFeature.HIGH.id) = PhoneFeatureValue.NEG
          features(PhoneFeature.LOW.id) = PhoneFeatureValue.POS
          if (desc.contains("near-open")) features(PhoneFeature.TENSE.id) = PhoneFeatureValue.POS
        }
        if (desc.contains("back")) features(PhoneFeature.BACK.id) = PhoneFeatureValue.POS
        if (desc.contains("front")) features(PhoneFeature.FRONT.id) = PhoneFeatureValue.POS
        features(PhoneFeature.ATR.id) = phone.ipa match {
          case "i" | "u" | "e" | "o" | "y" | "ø" => PhoneFeatureValue.POS
          case _ => PhoneFeatureValue.NEG
        }
      }
      case _ => {}
    }
    phone.ipa match {
      case "s" | "z" | "ʃ" | "ʒ" => {
        features(PhoneFeature.STRIDENT.id) = PhoneFeatureValue.POS
      }
      // Laryngeals
      case "h" | "ɦ" | "ɬ" => {
        features(PhoneFeature.SG.id) = PhoneFeatureValue.POS
      }
      case "ʔ" => {
        features(PhoneFeature.CG.id) = PhoneFeatureValue.POS
      }
      // Glides
      case "j" | "w" | "ɥ" |  "ɰ" => {
        features(PhoneFeature.CONS.id) = PhoneFeatureValue.NEG
        features(PhoneFeature.LABIAL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.DORSAL.id) = PhoneFeatureValue.POS
        features(PhoneFeature.HIGH.id) = PhoneFeatureValue.POS
        features(PhoneFeature.ROUND.id) = PhoneFeatureValue.NEG
        features(PhoneFeature.LOW.id) = PhoneFeatureValue.NEG
        features(PhoneFeature.BACK.id) = PhoneFeatureValue.NEG
        features(PhoneFeature.FRONT.id) = PhoneFeatureValue.NEG
        features(PhoneFeature.TENSE.id) = PhoneFeatureValue.NEG
        features(PhoneFeature.CONT.id) = PhoneFeatureValue.POS
        phone.ipa match {
          case "j" => {
          }
          case "w" => {
            features(PhoneFeature.BACK.id) = PhoneFeatureValue.POS
            features(PhoneFeature.ROUND.id) = PhoneFeatureValue.POS
          }
          case "ɥ" => {
            features(PhoneFeature.ROUND.id) = PhoneFeatureValue.POS
          }
          case "ɰ" => {
            features(PhoneFeature.BACK.id) = PhoneFeatureValue.POS
          }
          case _ => {}
        }
      }
      case _ => {}
    }
    features
  }

}

// Phonetic feature values are either +(positive),-(negative), or 0 unspecified
object PhoneFeatureValue extends Enumeration {
  type PhoneFeatureValue = Value
  val UNSPECIFIED = Value("0")
  val NEG = Value("-")
  val POS = Value("+")
  val POSNEG = Value("+-")
}

