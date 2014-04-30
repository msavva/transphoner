package org.babysherlock.transphoner.imageability

import org.babysherlock.transphoner.dict.{Language, Word}
import org.babysherlock.transphoner.Constants
import org.babysherlock.classify._
import org.babysherlock.nlp.UWN
import org.babysherlock.util._
import edu.stanford.nlp.util.logging.{Redwood, StanfordRedwoodConfiguration}
import java.io.File
import java.util.Properties
import scala.util.Random
import scala.util.parsing.json.{JSON, JSONArray}
import scala.collection.mutable

/**
 * Returns an imageability score from 0.0 to 1.0 (most imageable) for a word
 * @author Angel Chang
 */
trait Imageability {
  def apply(word: Word) = imageability(word)
  def apply(w: String, lang: Language) = imageability(w, lang)
  def imageability(word: Word): Double = imageability(word.orthographies.head, word.lang)
  def imageability(w: String, lang: Language): Double

  def mostImageable(words: Iterable[Word]) = words.toSeq.map( w => (w, imageability(w))).sortBy(-_._2)
  def leastImageable(words: Iterable[Word]) = mostImageable(words).reverse

  // Predict and print out imageability for all words in a language
  def printImageability(langId: String, outputFile: String) { printImageability(Language(langId), outputFile) }
  def printImageability(lang: Language, outputFile: String) { printImageability(lang.words, outputFile) }
  def printImageability(words: Set[Word], outputFile: String) {
    val output = IOUtils.filePrintWriter(outputFile)
    val wimgs = mostImageable(words)
    val img0 = wimgs.count(_._2 == 0)
    val imggt0 = wimgs.count(_._2 > 0)
    for ( (w,im) <- wimgs) {
      output.println(w.nativeWord + "\t" + im + "\t" + w.gloss)
    }
    output.close()
    println("no img: " + img0 + ", greater than 0: " + imggt0)
  }
}

object Imageability {
  val defaultImageability = new AoAImageability(true)
}


object PrintImageabilites extends App {
  val imageability = new AoAImageability(true)
  Language.setNumberOfGlossesToStore(1)
  for (lang <- Language.languages) {
    val imgFile = Constants.LANGS(lang)("img")
    imageability.printImageability(lang, imgFile)
  }
}

// Imageability using UWM
abstract class UWNImageability(// Set of uwn language codes for which we know the imagebility
                               val uwnImageabilityLangCodes: Set[String]
                               ) extends Imageability
{
  private val cache = LRUCache[(String,String),Double](100000)
  // Uses UWN to look up foreign words
  val uwn = UWN

  def lookupImageability(w: String, lang: String): Double

  // Returns the imageability of a word
  // For now look up foreign words
  override def imageability(w: String, lang: Language) = {
    imageability(w, lang.uwnCode)
  }
  def imageability(w: String, lang: String): Double = {
    cache.getOrElse((w,lang))(imageability_(w,lang))
  }

  def imageability_(w: String, lang: String): Double = {
    // TODO: account for word frequency
    if (uwnImageabilityLangCodes.contains(lang)) {
      // return imageability directly
      val im = lookupImageability(w, lang)
      if (im > 0) im
      else imageabilityFromSenses(w, lang)
    } else {
      imageabilityFromSenses(w, lang)
    }
  }

  def imageabilityFromSenses(word: Word): Double = {
    val str = word.orthographies.head
    imageabilityFromSenses(str, word.lang.uwnCode)
  }
  def imageabilityFromSenses(w: String, lang: Language): Double = imageabilityFromSenses(w, lang.uwnCode)
  def imageabilityFromSenses(w: String, lang: String): Double = {
    // Look up the senses of the word and return a weighted average of the imageability
    val synsets = uwn.lookupWeightedSynsetIds(w, lang)
    val synsetImageabilities = synsets.map(
      s => ( s._1, senseImageability(s._1), s._2 )
    )
    val sumWeights =  synsetImageabilities.map( x => x._3 ).sum
    val sumWeightedImaginabilities = synsetImageabilities.map( x => x._2 * x._3 ).sum
    if (sumWeights > 0) sumWeightedImaginabilities/sumWeights else 0
  }

  // Returns the imageability of a sense
  def senseImageability(senseId: String): Double = {
    // Lookup from imagenet counts
    // Lookup the imageability of all words associated with the sense
    val lexes = uwn.lookupSynsetLexicalizations(senseId,  uwnImageabilityLangCodes)
    // get word imageabilities and discard anything that is 0 (we don't know those...)
    val wordImageabilities = lexes.map(
      s => ( s.getObject.getTermStr, lookupImageability(s.getObject.getTermStr, s.getObject.getTermLanguage), s.getWeight )
    ).filter( t => t._2 > 0 )
    val sumWeights =  wordImageabilities.map( x => x._3 ).sum
    val sumWeightedImaginabilities = wordImageabilities.map( x => x._2 * x._3 ).sum
    if (sumWeights > 0) sumWeightedImaginabilities/sumWeights else 0
  }
}

//class ImageNetCounts {
//  lazy val imageNetCounts = IOUtils.loadMap(Constants.TRANSPHONER_DATA_DIR + "imagenetCounts.tsv", '\t')
//    .mapValues( x => x.toInt )
//  lazy val imageNetAggrCounts = IOUtils.loadMap(Constants.TRANSPHONER_DATA_DIR + "imagenetAggrCounts.tsv", '\t')
//    .mapValues( x => x.toInt )
//  def getImageNetCounts(senseId: String): Int = {
//    imageNetCounts.getOrElse(uwn.toWordNetSynsetId(senseId), 0)
//  }
//
//}

// Age of Acquisition information
case class AoAEntry(word: String,
                    pos: String,
                    freq: Double,
                    aoa: Double,
                    fam: Double,
                    nlet: Int,
                    nphon: Int,
                    nsyl: Int) {
  // TODO: put a sigmoid, incorporate POS
  def imageability = fam*1.0/aoa
}

trait ImageabilityPredictor {
  class MyDatum(val fields: Seq[String], // For print in tsv
                id: String,
                features: Map[String,Double],
                label: java.lang.Double) extends MappedDatum[java.lang.Double,String](id, features, label)

  val mrcWorkDir = Constants.WORK_DIR + "imageability" + File.separator + "mrc" + File.separator
  val trainDir = mrcWorkDir
  val testDir = mrcWorkDir
  val predictorDir = mrcWorkDir
  val features = Set(/*"nlet", "nphon", "nsyl",*/"fam", "bias", "faminvaoa", "invaoa", "aoa", "pos")
//  val features = Set(/*"nlet", "nphon", "nsyl",*/"bias", "invaoa", "aoa", "pos", "hypernyms")
//  val features = Set(/*"nlet", "nphon", "nsyl",*/"bias","synsets", "hypernyms")
  val saveFields = Seq("word","nlet", "nphon", "nsyl","freq", "fam", "bias", "faminvaoa", "invaoa", "aoa", "pos")
  val predictorName: String = "mrcImageabilityAoA"
  val serFormat = TrainedPredictor.SerializationFormat.BINARY

  type MyDataset = Dataset[MyDatum, java.lang.Double, String]
  class MyRegressor extends TrainedRegressorWithFeatureFactory[MyDatum,String] {
    private var myfeatures: Set[String] = features
    private var myfeaturizer = AoAEntryFeaturizer.featurize(myfeatures)_

    def valueOf(entry: AoAEntry): Double = {
      val datum = new MyDatum(Seq(), entry.word, myfeaturizer(entry), 0)
      valueOf(datum)
    }

    override def saveCustomFields(customFields: java.util.Map[java.lang.String,java.lang.String]) {
      customFields.put("features", JSONArray.apply(myfeatures.toList).toString())
    }

    // Overload to load custom fields
    override def loadCustomFields(customFields: java.util.Map[java.lang.String,java.lang.String]) {
      val f = customFields.get("features")
      if (f != null) {
        myfeatures = JSON.parseFull(f).get.asInstanceOf[List[String]].toSet
        myfeaturizer = AoAEntryFeaturizer.featurize(myfeatures)_
      }
    }
  }

  def getPredictorFile(predictorName: String) = predictorDir + predictorName + "." + serFormat.extension + ".gz"
  def loadRegressor(s: String) = {
    val props = new Properties()
    props.setProperty("loadPredictor", s)
    props.setProperty("serFormat", serFormat.name())
    val predictor = new MyRegressor()
    predictor.init(props,null)
    predictor.loadPredictor()
    predictor.setFeatureFactory(new MappedDatumCreator[MyDatum,java.lang.Double,String])
    predictor
  }
}

object ImageabilityPredictor extends ImageabilityPredictor

// Imagebility using age of acquisition
class AoAImageability(val aoafilename: String = Constants.LANGS("EN")("aoa"),
                      val regressorFilename: Option[String] = None) extends UWNImageability(Set("eng")) with ImageabilityPredictor {
  val aoaMap = loadAoACsv(aoafilename)
  val aoaRegressor: Option[MyRegressor] = regressorFilename.map( r => loadRegressor(r) )

  //ImageabilityPredictor.getPredictorFile(ImageabilityPredictor.predictorName)
  def this(useRegressor: Boolean) = this(Constants.LANGS("EN")("aoa"),
    if (useRegressor) Option(Constants.TRANSPHONER_DATA_DIR + "mrcImageability.pred.gz") else None)

  override def lookupImageability(w: String, lang: String): Double = {
    if (lang == "eng") {
      val entry = aoaMap.get(w)
      if (entry.isDefined) {
        if (aoaRegressor.isDefined) {
          //entry.get.fam *
            aoaRegressor.get.valueOf(entry.get)
        }
        else entry.get.imageability
      } else 0
    } else 0
  }

  // Loads Age of Acquisition csv from Kuperman et al
  def loadAoACsv(filename: String): Map[String, AoAEntry] = {
    val csvFile = new CSVFile(filename, includesHeader = true)
    val myMap = (for (row <- csvFile) yield {
      val r = new CSVRow(row, csvFile)
      val freqStr = r("Freq_pm").trim()
      val aoaStr = r("AoA_Kup_lem").trim()
      val word = r("Word").trim()
      word -> AoAEntry(
        word = word,
        pos = r("Dom_PoS_SUBTLEX").trim().intern(),
        freq = if ("#N/A" == freqStr) 0 else freqStr.toDouble,
        aoa = if ("NA" == aoaStr) Double.NaN else aoaStr.toDouble,
        fam = r("Perc_known_lem").trim().toDouble,
        nlet = r("Nletters").trim().toInt,
        nphon = r("Nphon").trim().toInt,
        nsyl = r("Nsyll").trim().toInt
      )
    }).toMap
    myMap.filter(x => x._2.aoa != Double.NaN)
  }
}

object AoAEntryFeaturizer extends org.babysherlock.classify.Featurizer[AoAEntry] {
  override val featureMap = Map[String, (AoAEntry) => Any ](
    // numeric features
    "nlet" -> (x => x.nlet),
    "nphon" -> (x => x.nphon),
    "nsyl" -> (x => x.nsyl),
    "freq" -> (x => x.freq),
    "aoa" -> (x => x.aoa),
    "invaoa" -> (x => 1.0/x.aoa),
    "faminvaoa" -> (x => x.fam/x.aoa),
    "fam" -> (x => x.fam),
    "synsets" -> (x => getSynsets(x.word)),
    "hypernyms" -> (x => getHypernymSynsets(x.word)),

    // non-numeric features
    "pos" -> (x => x.pos),
    "word" -> (x => x.word),

    // Bias term
    "bias" -> (x => 1)
  )
  // TODO: Add freqrank (have list of entries and get freq rank from there

  def getSynsets(w: String): Seq[(String,Double)] = {
    UWN.lookupWeightedSynsetIds(w, "eng")
  }
  def getHypernymSynsets(w: String): Seq[(String,Double)] = {
    val synsets = getSynsets(w)
    synsets.flatMap(
      s => {
        val ancestors = UWN.lookupAncestorSynsetsIds(s._1)
        ancestors.map(a => (a,s._2))
      }).groupBy( s => s._1 ).mapValues( s => s.map( s => s._2 ).sum ).toSeq
  }
}

object ImageNetCountImages extends App {
  val imagesUrlsFile = Constants.EXT_DATA_DIR + "imagenet" + File.separator + "fall11_urls.txt"
  val outputFile = Constants.TRANSPHONER_DATA_DIR + "imagenetCounts.tsv"
  val lines = IOUtils.fileSource(imagesUrlsFile, charset="ISO-8859-1").getLines()
  val counts = new mutable.HashMap[String,Int]()
  for (line <- lines) {
    val synsetid = line.split("\\s+")(0).split("_")(0)
    val old = counts.getOrElseUpdate(synsetid, 0)
    counts.update(synsetid, old+1)
  }
  IOUtils.saveList(outputFile, counts.toSeq.sortBy( -_._2 ).map( x => x._1 + "\t" + x._2) )

  // Get aggregate counts for each synset
  val aggrOutputFile = Constants.TRANSPHONER_DATA_DIR + "imagenetAggrCounts.tsv"
  val aggrCounts = new mutable.HashMap[String,Int]()
  for ( (synsetid, cnt) <- counts ) {
    val ancestors = UWN.lookupAncestorSynsetsIds(UWN.toUWNSenseId(synsetid)).map( x => UWN.toWordNetSynsetId(x))
    for (s <- ancestors ++ Seq(synsetid)) {
      val old = aggrCounts.getOrElseUpdate(s, 0)
      aggrCounts.update(s, old+cnt)
    }
  }
  IOUtils.saveList(aggrOutputFile, aggrCounts.toSeq.sortBy( -_._2 ).map( x => x._1 + "\t" + x._2) )

  // Get sense imageability
  val senseOutputFile = Constants.TRANSPHONER_DATA_DIR + "wordnetSenseImg.tsv"
  val senseImg = counts.keySet.map( s => {
    val uwnSenseId = UWN.toUWNSenseId(s)
    (s, Imageability.defaultImageability.senseImageability(uwnSenseId),
      UWN.lookupSynsetLexicalizations(uwnSenseId, Set("eng")).map( x => x.getObject.getTermStr ).mkString(", "),
      UWN.lookupGlosses(uwnSenseId).map( x => x.getObject.getTermStr ).mkString("; ") )
  })
  IOUtils.saveList(senseOutputFile, senseImg.toSeq.sortBy( -_._2 ).map( x => x._1 + "\t" + x._2 + "\t" + x._3 + "\t" + x._4) )
}

object MrcImagineabilityTrainer extends App with ImageabilityPredictor {
  override val serFormat = TrainedPredictor.SerializationFormat.JSON
  val reader = MrcDctReader
  val allMrcEntries = reader.entries().toSeq
  val mrcEntriesWithImageability = allMrcEntries.filter( x => x.imag > 0 )
  val aoaImageability = new AoAImageability()
  val aoaEntries = mrcEntriesWithImageability.map( x => (aoaImageability.aoaMap.getOrElse(x.word.toLowerCase, null),
    (x.imag.toDouble-100)/600) )
    .filter( e => e._1 != null).distinct
  println("aoaentries size: " + aoaEntries.size)

  val dirs = Seq(mrcWorkDir)
  IOUtils.createDirs(dirs)

  val (trainEntries,testEntries) = aoaEntries.partition( e => Random.nextDouble() < 0.8 )
  trainAndEvaluate(predictorName + "8020",  saveFields, features, trainEntries, testEntries)
  trainAndEvaluate(predictorName, saveFields, features, trainEntries ++ testEntries, Seq())
  // Try loading saved predictor (assumes things are okay if it loads)
  loadRegressor( getPredictorFile(predictorName) )


  def trainAndEvaluate(predictorName: String, saveFields: Seq[String], featureSet: Set[String],
                       trainEntries: Seq[(AoAEntry,Double)], testEntries: Seq[(AoAEntry,Double)]): MyRegressor = {
    val featurizer = AoAEntryFeaturizer.featurize(features)_
    val featureValuesAsStrings = AoAEntryFeaturizer.featureValuesAsStrings(saveFields)_

    val loggingProps = new Properties
    loggingProps.setProperty("log.file", trainDir + predictorName + ".train.log")
    loggingProps.setProperty("log.captureStderr", "true")
    StanfordRedwoodConfiguration.apply(loggingProps)
    Redwood.startTrack("train " + predictorName)

    val trainDatums = trainEntries.map( e => new MyDatum(featureValuesAsStrings(e._1), e._1.word, featurizer(e._1), e._2) )
    val testDatums = testEntries.map( e => new MyDatum(featureValuesAsStrings(e._1), e._1.word, featurizer(e._1), e._2) )
    val trainData = Dataset[MyDatum,java.lang.Double,String](trainDatums)
    val testData = Dataset[MyDatum,java.lang.Double,String](testDatums)

    val trainProps = new Properties()
    trainProps.setProperty("savePredictor", getPredictorFile(predictorName))
    trainProps.setProperty("printPredictorInfo", "HighMagnitude")
    trainProps.setProperty("featureCountThreshold", "2")
    trainProps.setProperty("serFormat", serFormat.name())
    trainData.save(trainDir + predictorName + ".train.txt")
    val predictor = train(predictorName, trainData, trainProps)
    evaluate("Train", predictor, trainData, trainDir + predictorName + "-train")
    evaluate("Test", predictor, testData, testDir + predictorName + "-test")

    Redwood.endTrack("train " + predictorName)
    loggingProps.remove("log.file")
    StanfordRedwoodConfiguration.apply(loggingProps)

    predictor
  }

  def train(predictorId: String,
            trainingData: MyDataset,
            properties: Properties = new Properties()):MyRegressor  = {
    val predictor = new MyRegressor()
    predictor.setPredictorId(predictorId)
    val trainer = new RegressorTrainer[MyDatum,String,MyRegressor](predictor)
    trainer.init(properties)
    trainer.trainData = trainingData
    //trainer.trainClassifier(trainingData.dataset)
    trainer.setupPredictor()
    trainer.evaluateTestset()
    trainer.printPredictor()
    predictor
  }

  def evaluate(name: String, predictor: MyRegressor, testData: MyDataset, output: String = null): Double = {
    val score = predictor.predictAndPrintTsv(name, testData, output, true,
      new edu.stanford.nlp.util.Function[MyDatum, Array[String]]() {
        def apply(m: MyDatum): Array[String] = {
          m.fields.toArray
        }
      },
      saveFields.toArray, "\t"
    )
    score
  }

}

