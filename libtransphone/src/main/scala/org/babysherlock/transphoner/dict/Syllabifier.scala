package org.babysherlock.transphoner.dict

import org.babysherlock.transphoner.TrieHelper
import org.babysherlock.transphoner.phonetics.IPA
import org.babysherlock.util.IOUtils
import edu.stanford.nlp.tokensregex.matcher.TrieMapMatcher
import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Syllabifier
 * See http://en.wikipedia.org/wiki/Syllable
 * @author Angel Chang
 */
trait Syllabifier {
  def syllabify(ipa: String): String
}

class PhonotacticSyllabifier(val syllFilename: String) extends Syllabifier {
  object SyllablePart extends Enumeration  {
    type SyllablePart = Value
    val onsets, nuclei, codas, consonants, breaks, links, stresses = Value
  }

  // Consider all suprasegmentals group markers and syllable separator as syllable breaks
  val syllableBreaks = IPA.groupMarkers.map(x => x.ipa).toSet + IPA.syllableSep.ipa
  // stresses go before the syllable
  val stresses = IPA.stresses.map( x => x.ipa ).toSet
  // links shouldn't break a syllable
  val links = IPA.links.map( x => x.ipa ).toSet
  val syllableParts = readSyllableFile(syllFilename)
  private val charTrie = TrieHelper.pairsToCharTrie( syllableParts.map( x => x._2.toSeq.map( y => (y, x._1) )).flatten )

  private def readSyllableFile(filename: String): Map[SyllablePart.Value, Set[String]] = {
    val myMap = new mutable.HashMap[SyllablePart.Value, mutable.Set[String] ]
    var mySet: mutable.Set[String] = null
    val lines = IOUtils.getLines(filename).map( line => line.trim() )
    var lineNo = 0
    for (line <- lines) {
      lineNo = lineNo + 1
      if (line.nonEmpty) {
        if (line.endsWith(":")) {
          // Starting a new section
          val syllablePart = line match {
            case "onsets:" => SyllablePart.onsets
            case "nuclei:" => SyllablePart.nuclei
            case "codas:" => SyllablePart.codas
            case "consonants:" => SyllablePart.consonants
            case _ =>  throw new RuntimeException("Unexpected syllabifier line (" + filename + ":" + lineNo + ") - unknown section: " + line)
          }
          mySet = myMap.getOrElseUpdate( syllablePart, new mutable.HashSet[String]())
        } else {
          if (mySet == null) {
            throw new RuntimeException("Unexpected syllabifier line (" + filename + ":" + lineNo + ") - not in section: " + line)
          }
          val separatorIndex = line.indexOf('-')
          if (separatorIndex >= 0) {
            val symbol = line.substring(separatorIndex+1).trim()
            mySet += symbol
          } else {
            println("WARNING: Unexpected syllabifier line (" + filename + ":" + lineNo + "): " + line)
          }
        }
      }
    }
    myMap.mapValues( s => s.toSet ).toMap ++ Map(SyllablePart.breaks -> syllableBreaks,
      SyllablePart.links -> links, SyllablePart.stresses -> stresses)
  }

  private def hasVowel(chars: Seq[Char]): Boolean = {
    val str = chars.mkString
    val phones = IPA.strToPhoneSeq(str)
    phones.exists( p => p.isVowel)
  }

  def syllabify(ipa: String): String = {
    // Look for onset, nuclei, coda matches
    val matcher = new TrieMapMatcher(charTrie)
    // Simplified syllabifier using segment
    val segments = matcher.segment(ipa.toSeq)
    val putBreakAt = new mutable.TreeSet[Integer]

    // Go through segments figure out segment indices where the syllable breaks should go
    var lastIsBreak = false
    var lastIsOnset = false
    var lastIsLink = false
    var lastIsNuclei = false
    var hasNuclei = false
    for ( (segment, index) <- segments.zipWithIndex) {
      val segmentTypes = if (segment.getValue != null) segment.getValue else Set()
      val isOnset = segmentTypes.contains(SyllablePart.onsets) || segmentTypes.contains(SyllablePart.consonants)
      val isNuclei = segmentTypes.contains(SyllablePart.nuclei) || (segmentTypes.isEmpty && hasVowel(segment.getMatched))
      //val isCoda = segmentTypes.contains(SyllablePart.consonants) || segmentTypes.contains(SyllablePart.codas)
      val isBreak = segmentTypes.contains(SyllablePart.breaks)
      val isStress = segmentTypes.contains(SyllablePart.stresses)
      val isLink = segmentTypes.contains(SyllablePart.links)

      // Only put breaks if we had already encountered a nuclei
      if (!lastIsLink && !isLink && hasNuclei) {
        if (isStress || (lastIsBreak && !isBreak)) {
          // this definitely a syllable start that needs a break
          // only put break if this indicates stress marker (otherwise, should already have break)
          if (isStress && !lastIsBreak) putBreakAt += index
          hasNuclei = false
        } else if (isNuclei && lastIsOnset) {
          // the last index was definitely a syllable start
          putBreakAt += (index-1)
        } else if (isNuclei && lastIsNuclei) {
          // this index is probably a syllable start
          putBreakAt += index
        }
      }
      lastIsBreak = isBreak
      lastIsOnset = isOnset
      lastIsLink = isLink
      lastIsNuclei = isNuclei
      hasNuclei = hasNuclei || isNuclei
    }

    var str = ""
    for ( (segment, index) <- segments.zipWithIndex) {
      val putBreak = putBreakAt.contains(index)
      val m = segment.getMatched.mkString
      str = if (putBreak) str + IPA.syllableSep.ipa + m else str + m
    }

    str
  }
}


