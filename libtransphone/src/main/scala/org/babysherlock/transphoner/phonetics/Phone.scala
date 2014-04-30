package org.babysherlock.transphoner.phonetics

import scala.io.Codec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import org.babysherlock.transphoner.Constants


/**
 * International Phonetic Alphabet (IPA) classes
 * @author Manolis Savva
 */

/**
 * Represents a phone (segment) of the IPA.
 *   ipa = Representation in UTF-8 IPA encoding
 *   xsampa = Representation in ASCII X-SAMPA encoding
 *   category = Category
 */
case class Phone(ipa: String, xsampas: Seq[String] = null, asjp: String = null, category: PhoneCategory.Value = PhoneCategory.NULL,
                 description: String = null, altIPA: List[String] = null) {
  // TODO: Get features from file
  lazy val xsampa = xsampas.head
  lazy val basicFeatures: Array[PhoneFeatureValue.Value] = PhoneFeature.features(this)
  lazy val alineFeatures: Array[Double] = AlinePhoneFeature.features(this)
  override val toString: String = ipa
  override def equals(that: Any): Boolean =
    that.isInstanceOf[Phone] && (this.ipa == that.asInstanceOf[Phone].ipa)
  override def hashCode(): Int = ipa.hashCode
  def equals(thatIpa: String): Boolean = ipa == thatIpa || altIPA.contains(thatIpa)
  def stripFrom(s: String): String = {
    val matchedIpa = (ipa +: altIPA).find(s.startsWith).getOrElse(null)
    s.stripPrefix(matchedIpa)
  }
  lazy val isTranscribeable: Boolean = PhoneCategory.TRANSCRIBEABLE.contains(category)
  lazy val isPronounceable: Boolean = PhoneCategory.PRONOUNCEABLE.contains(category)
  lazy val isVowel: Boolean = PhoneCategory.VOWEL == category
  lazy val isConsonant: Boolean = PhoneCategory.CONSONANT.contains(category)
  // Includes suprasegmentals for long, half long, extra-short,
  lazy val isDiacritic: Boolean = PhoneCategory.DIACRITIC == category
  lazy val isDiacriticLike: Boolean = isDiacritic || isLengthMarker
  lazy val isLengthMarker = ipa == IPA.SYM_HALF_LONG || ipa == IPA.SYM_LONG || ipa == IPA.SYM_EXTRA_SHORT
  lazy val isSuprasegmental: Boolean = PhoneCategory.SUPRASEGMENTAL == category
  lazy val isGroupMarker: Boolean = ipa == IPA.SYM_GROUP_MAJOR || ipa == IPA.SYM_GROUP_MINOR
  lazy val isLink: Boolean = ipa == IPA.SYM_LINK
  lazy val isStressMarker: Boolean = ipa == IPA.SYM_PRIMARY_STRESS || ipa == IPA.SYM_SECONDARY_STRESS
  lazy val isTone: Boolean = getBasePhone.description.contains("tone")
  lazy val hasTone: Boolean = diacritics.exists( d => d.isTone )
  def tones: Seq[Phone] = diacritics.filter( d => d.isTone )
  def isGlottalStop: Boolean = PhoneCategory.STOP == category
  def isPrimaryStress: Boolean = ipa == IPA.SYM_PRIMARY_STRESS
  def isSecondaryStress: Boolean = ipa == IPA.SYM_SECONDARY_STRESS
  lazy val isSyllableSep: Boolean = PhoneCategory.SYLLABLE_SEP == category
  def diacritics = Seq[Phone]()
  def hasDiacritic: Boolean = false
  def getBasePhone = this
  def addDiacritic(diacritic: Phone): Phone = new PhoneWithDiacritic(this, diacritic)
  lazy val place = place1
  lazy val (place1,place2) = PhoneArticulationPlace.fromDescription(description)
}

class PhoneWithDiacritic(val basePhone: Phone, override val diacritics: Seq[Phone])
  extends Phone(basePhone.ipa + diacritics.map(p => p.ipa).mkString(""),
                Seq(basePhone.xsampa + diacritics.map(p => p.xsampa).mkString("")),
                basePhone.asjp, basePhone.category,
                basePhone.description.concat(" with ").concat(diacritics.mkString(",")))
{
  // Add diacritic features to phone feature
  override lazy val basicFeatures: Array[PhoneFeatureValue.Value] = {
    val baseFeatures = basePhone.basicFeatures
    val features = baseFeatures.clone()
    for (d <- diacritics) {
      val df = d.basicFeatures
      for (i <- 0 until PhoneFeature.maxId) {
        if (df(i) != PhoneFeatureValue.UNSPECIFIED) {
          features(i) = df(i)
        }
      }
    }
    features
  }
  def this(basePhone: Phone, diacritic: Phone) = this(basePhone, Seq(diacritic))
  override def getBasePhone = basePhone
  override def addDiacritic(diacritic: Phone): Phone =
    new PhoneWithDiacritic(basePhone, diacritics :+ diacritic)
  override def hasDiacritic: Boolean = true
}

case class Tone(name: String,
                ipa: String,
                description: String,
                stressType: StressType.Value = StressType.none) {
  val phones = IPA.strToPhoneSeq(ipa)
}

object StressType extends Enumeration {
  type StressType = Value
  val primary, secondary, none = Value
  def toIPA(s: Value): String = {
    if (s == primary) IPA.SYM_PRIMARY_STRESS
    else if (s == secondary) IPA.SYM_SECONDARY_STRESS
    else ""
  }
  def valueOf(s: String): Value = {
    if (s == null || s.isEmpty) none else withName(s)
  }
}

object PhoneCategory extends Enumeration {
  type PhoneCategory = Value
  val VOWEL = Value("vowel")
  val AFFRICATE = Value("affricate")
  val APPROXIMANT = Value("approximant")
  val FLAP = Value("flap")
  val FRICATIVE = Value("fricative")
  val PLOSIVE = Value("plosive")
  val IMPLOSIVE = Value("implosive")
  val NASAL = Value("nasal")
  val TRILL = Value("trill")
  val DIACRITIC = Value("diacritic")
  val SUPRASEGMENTAL = Value("suprasegmental")
  val CLICK = Value("click")
  val STOP = Value("stop")
  val SYLLABLE_SEP = Value("syllablesep")
  val NULL = Value("null")

  val CONSONANT = ValueSet(AFFRICATE, APPROXIMANT, FLAP, FRICATIVE, PLOSIVE, IMPLOSIVE, NASAL, TRILL, CLICK, STOP)
  val PRONOUNCEABLE = ValueSet(VOWEL, AFFRICATE, APPROXIMANT, FLAP, FRICATIVE, PLOSIVE, IMPLOSIVE, NASAL, TRILL, CLICK, STOP)
  val NONPRONOUNCEABLE = ValueSet(DIACRITIC, SUPRASEGMENTAL, SYLLABLE_SEP, NULL)
  val TRANSCRIBEABLE = PRONOUNCEABLE + SYLLABLE_SEP
}

object PhoneArticulationPlace extends Enumeration {
  type PhoneArticulationPlace = Value
  val Labial = Value("Labial")
  val Labiodental = Value("Labiodental")
  val Dental = Value("Dental")
  val Alveolar = Value("Alveolar")
  val Retroflex = Value("Retroflex")
  val Postalveolar = Value("Postalveolar")
  val Palatal = Value("Palatal")
  val Velar = Value("Velar")
  val Uvular = Value("Uvular")
  val Pharyngeal = Value("Pharyngeal")
  val Glottal = Value("Glottal")

  // not covered: bilabial,  epiglottal

  // goes from description to place of articulation
  def fromDescription(desc: String): (PhoneArticulationPlace, PhoneArticulationPlace) = {
    desc match {
      case d if d.contains("alveolo-palatal") => (Alveolar, Palatal)
      case d if d.contains("labial-velar") => (Labial, Velar)
      case d if d.contains("labial-palatal") => (Labial, Palatal)
      case _ => {
        val place = desc match {
          case d if d.contains("labial") => Labial
          case d if d.contains("labiodental") => Labiodental
          case d if d.contains("dental") => Dental
          case d if d.contains("postalveolar") => Postalveolar
          case d if d.contains("alveolar") => Alveolar
          case d if d.contains("retroflex") => Retroflex
          case d if d.contains("palatal") => Palatal
          case d if d.contains("velar") => Velar
          case d if d.contains("uvular") => Uvular
          case d if d.contains("pharyngeal") => Pharyngeal
          case d if d.contains("glottal") => Glottal
          case _ => null
        }
        (place,null)
      }
    }
  }
}

/**
 * Represents a null Phone
 */
object PhoneNull extends Phone("∅", Seq("*"), "*", PhoneCategory.NULL, "NULL") {
  override def equals(that: Any): Boolean = false
}

/**
 * Represents the entire IPA as a list of Phones. Provides convenience functions.
 */
object IPA {
  val phones: List[Phone] = io.Source.fromFile(Constants.IPA_CSV)(codec = Codec.UTF8).getLines().drop(1).map(l => {
    val row = l.split(',')
    val ipaStr = row(0).trim()
    val xsampas = row(1).trim().split(" +").toSeq
    val category = row(2)
    val description = row(3)
    val asjp = if (row.size > 4) row(4) else null
    val ipas = ipaStr.split(" +").toList
    val altIpas = if (ipas.size > 1)
      ipas.drop(1)
    else List()
    Phone(ipas(0), xsampas, asjp, PhoneCategory.withName(category), description, altIpas)
  }).toList //.sortBy(_.category)
  private val phonesWithDiacritics = new mutable.HashMap[String,Phone]()
  private val mainStrToPhone = phones.map(p => p.ipa -> p).toMap
  private val altStrToPhone = phones.map(p => p.altIPA.map(ipa => ipa -> p)).flatten.toMap
  private val strToPhone = mainStrToPhone ++ altStrToPhone

  lazy val syllableSep = phones.find( p => p.isSyllableSep ).get
  lazy val suprasegmentals = phones.filter( p => p.isSuprasegmental )
  lazy val diacriticLikes = phones.filter( p => p.isDiacriticLike )
  lazy val groupMarkers = phones.filter( p => p.isGroupMarker )
  lazy val stresses = phones.filter( p => p.isStressMarker )
  lazy val links = phones.filter( p => p.isLink )
  lazy val vowels = phones.filter( p => p.isVowel )
  lazy val consonants = phones.filter( p => p.isConsonant )

  val phonesByType = phones.groupBy(_.category)

  // Some special IPA symbols (mainly suprasegmentals) that we use
  // IPA is tricky, things look very similar
  val SYM_HALF_LONG = "ˑ"
  val SYM_LONG = "ː"
  val SYM_EXTRA_SHORT = "̆"
  val SYM_GROUP_MAJOR = "‖"
  val SYM_GROUP_MINOR = "|"
  val SYM_LINK = "‿"
  val SYM_PRIMARY_STRESS = "ˈ"
  val SYM_SECONDARY_STRESS = "ˌ"
  val SYM_SYLLABLE_BREAK = "."

  def isPhone(x: String): Boolean = phones.exists(p => p == x)

  def strToPhoneSeq(s: String, nonIPA: mutable.HashMap[Char,Int] = null): Seq[Phone] = {
    var remain = s
    val matched = new ArrayBuffer[Phone]()
    var last: Phone = null
    while (remain.length > 0) {
      val x = strToPhone.getOrElse(remain.take(3),
              strToPhone.getOrElse(remain.take(2),
              strToPhone.getOrElse(remain.take(1),
              null)))
      if (x != null) {
        if (x.isDiacriticLike) {
          // is a diacritic (should be aggregated with previous phone to create new big phone)
          if (last != null) {
            val lastWithDiacritic = last.addDiacritic(x)
            matched.reduceToSize(matched.size - 1)
            last = phonesWithDiacritics.getOrElseUpdate(lastWithDiacritic.ipa, lastWithDiacritic)
            //println("Got " + lastWithDiacritic + " in " + s)
          } else {
            //println("Nothing before diacritic " + x + " in " + s + "!!!")
            last = x
          }
        } else {
          last = x
        }
        matched.append(last)
        remain = x.stripFrom(remain)
      }
      // No prefix word so remove one character, record it as unrecognized and proceed
      else {
        val charDropped = remain.charAt(0)
        if (nonIPA != null) nonIPA(charDropped) = nonIPA.getOrElseUpdate(charDropped,0) + 1
        remain = remain.drop(1)
      }
    }
    //if (!matched.forall(p => !p.hasDiacritic))
    //println(s + " -> " + matched.filter(_.isPronounceable).mkString(""))
    val filtered = matched.filter( p => p.isTranscribeable || p.isStressMarker)
    val finalMatched = Array.ofDim[Phone](filtered.length)
    filtered.copyToArray(finalMatched)
    finalMatched.toSeq
  }

}