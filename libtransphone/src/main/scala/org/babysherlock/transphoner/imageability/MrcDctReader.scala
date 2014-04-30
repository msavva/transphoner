package org.babysherlock.transphoner.imageability

import org.babysherlock.transphoner.Constants
import org.babysherlock.util.IOUtils

/**
 * Reads a MRC DCT file
 * Format is specified in http://www.psych.rl.ac.uk/MRC_Psych_Db_files/mrc2.html
 * @author Angel Chang
 */
object MrcDctReader {
  val defaultMrcDctFile = Constants.LANGS("EN")("mrc")

  object Featurizer extends org.babysherlock.classify.Featurizer[Entry] {
    override val featureMap = Map[String, (Entry) => Any ](
      // Integer features
      "nlet" -> (x => x.nlet),
      "nphon" -> (x => x.nphon),
      "nsyl" -> (x => x.nsyl),
      "kffreq" -> (x => x.kffreq),
      "kfncats" -> (x => x.kfncats),
      "kfnsamp" -> (x => x.kfnsamp),
      "tlfreq" -> (x => x.tlfreq),
      "brownfreq" -> (x => x.brownfreq),
      "fam" -> (x => x.fam),
      "conc" -> (x => x.conc),
      "imag" -> (x => x.imag),
      "meanc" -> (x => x.meanc),
      "meanp" -> (x => x.meanp),
      "aoa" -> (x => x.aoa),

      // non-numeric features
      "tq2" -> (x => x.tq2),
      "wtype" -> (x => x.wtype),
      "pdwtype" -> (x => x.pdwtype),
      "alphsyl" -> (x => x.alphsyl),
      "status" -> (x => x.status),
      "varphon" -> (x => x.varphon),
      "cap" -> (x => x.cap),
      "irreg" -> (x => x.irreg),

      "word" -> (x => x.word),
      "phon" -> (x => x.phon),
      "dphon" -> (x => x.dphon),
      "stress" -> (x => x.stress)
    )
  }

  case class Entry(properties: String,
                   fields: Array[String]) {
    def nlet = intProp(1,2)
    def nphon = intProp(3,4)
    def nsyl = intProp(5)
    def kffreq = intProp(6,10)
    def kfncats = intProp(11,12)
    def kfnsamp = intProp(13,15)
    def tlfreq = intProp(16,21)
    def brownfreq = intProp(22,25)
    def fam = intProp(26,28)
    def conc = intProp(29,31)
    def imag = intProp(32,34)
    def meanc = intProp(35,37)
    def meanp = intProp(38,40)
    def aoa = intProp(41,43)
    def tq2 = charProp(44)
    def wtype = charProp(45)
    def pdwtype = charProp(46)
    def alphsyl = charProp(47)
    def status = charProp(48)
    def varphon = charProp(49)
    def cap = charProp(50)
    def irreg = charProp(51)
    def word = fields(0)
    def phon = fields(1)
    def dphon = fields(2)
    def stress = fields(3)

    private def intProp(start: Int, end: Int): Int = properties.substring(start-1, end).toInt
    private def intProp(start: Int): Int = intProp(start, start)
    private def charProp(start: Int) = properties.charAt(start-1)

    override def toString: String = {
      word + ": " + properties + "," + fields.mkString(",")
    }
  }

  def toEntry(line: String): Entry = {
    val properties = line.substring(0,51)
    val rest = line.substring(51)
    val fields = rest.split("\\|")
    Entry(properties, fields)
  }

  def entries(filename: String = defaultMrcDctFile) = {
    IOUtils.getLines(filename).map( line => toEntry(line) )
  }
}

object MrcDctReaderTest extends App {
  val reader = MrcDctReader
  val all = reader.entries().toSeq
  val mapped = all.groupBy( x => x.word )
  val hasImageability = all.filter( x => x.imag > 0 )
  val featurizer = reader.Featurizer.featurize(Set("word", "nlet", "wtype", "imag", "aoa"))_
  val testWords = Set( "WATER" )
  val test = all.filter( x => testWords.contains( x.word ) )
  println(test.mkString("\n"))
  println(test.map( x => reader.Featurizer.featuresCountsToString( featurizer(x) ) ).mkString("\n"))
  println("all=" + all.size + ", withImageability=" + hasImageability.size)
}