package org.babysherlock.classify;

import edu.stanford.nlp.classify.*;

import java.io.File;
import java.util.Properties;

/**
 * Wrapper with basic flags for training and testing predictors
 *
 * To train predictor:
 * <table border="1">
 * <tr><td><b>Property Name</b></td><td><b>Type</b></td><td><b>Default Value</b></td><td><b>Description</b></td></tr>
 * <tr><td>trainFile</td><td>String</td><td></td><td>Name of file with training instances (format will depend on classifier).</td></tr>
 * <tr><td>trainFeatsFile</td><td>String</td><td></td><td>If specified, training features will be output to this file.</td></tr>
 * <tr><td>testFile</td><td>String</td><td></td><td>Name of file with test instances (format will depend on classifier).</td></tr>
 * <tr><td>testFeatsFile</td><td>String</td><td></td><td>If specified, test features will be output to this file.</td></tr>
 * <tr><td>testOutFile</td><td>String</td><td></td><td>If specified, labels for the test instances will be output to this file.</td></tr>
 * <tr><td>savePredictor</td><td>String</td><td></td><td>If specified, trained classifier will be saved to this file.</td></tr>
 * <tr><td>predictorType</td><td>String</td><td>LINEAR</td><td>Type of classifier to train: LINEAR, SVMLIGHT, KNN</td></tr>
 * <tr><td>evaluateIters</td><td>int</td><td>0</td><td>If this number is greater than 0, evaluates on the test set every so often while minimizing.</td></tr>
 * <tr><td>l1reg</td><td>double</td><td>0.0</td><td>If set to be larger than 0, uses L1 regularization</td></tr>
 * </table>
 *
 * To use trained predictor:
 * <table border="1">
 * <tr><td>loadPredictor</td><td>String</td><td></td><td>Filename of serialized predictor</td></tr>
 * <tr><td>testFile</td><td>String</td><td></td><td>Name of file with test instances (format will depend on predictor).</td></tr>
 * <tr><td>testFeatsFile</td><td>String</td><td></td><td>If specified, test features will be output to this file.</td></tr>
 * <tr><td>testOutFile</td><td>String</td><td></td><td>If specified, labels for the test instances will be output to this file.</td></tr>
 * </table>
 *
 * Feature flags (applies to both training and running predictor)
 * <table border="1">
 * <tr><td>loadFeatures</td><td>String</td><td></td><td>If specified, features will be loaded from this file (instead of being recomputed from input instances.  Features should be matched to a input  instance by a lookup key.</td></tr>
 * <tr><td>saveFeatures</td><td>String</td><td></td><td>If specified, features will be saved to this file so that they don't have to be recomputed.  The file is a binary object (map of lookup key to features)</td></tr>
 * </table>
 *
 * @author Angel Chang
 */
abstract public class PredictorTrainer<P, S extends PredictorStats<L>, E, L, F, TP extends TrainedPredictor<P,S,E,L,F>> {
  // Test/Train data
  public DatasetInfo<E,L,F> testData;
  public DatasetInfo<E,L,F> trainData;

  public TP predictor;

  public Flags flags;

  public enum PredictorType {LINEAR, SVMLIGHT, SVM, KNN, BAGGING, WEKA, NAIVE_BAYES}

  public PredictorTrainer(TP predictor)
  {
    this.predictor = predictor;
  }

  public TP getPredictor() {
    return predictor;
  }

  public static class Flags {
    public TrainedPredictor.SerializationFormat serFormat = TrainedPredictor.SerializationFormat.BINARY;
    public int serFormatVersion = 1;
    public boolean mergeTrainFiles; // Multiple train files may be specified, need to merge them
    public String[] trainFiles;
    public String trainFeatsFile;
    public String trainFile;
    public String testFeatsFile;
    public String testFile;
    public String testOutFile;
    public String loadFeatures;
    public String saveFeatures;
    public String loadPredictor;
    public String savePredictor;
    public boolean useProbabilityAsScore;

    // NOTE: Be very careful about setting this flag, it will cause the trainfile to be DELETED
    //       during cleanup!!!!
    public boolean deleteTrainFile = false;

    String printPredictorFile;
    String printPredictorInfo;  // HighWeight, HighMagnitude, AllWeights, WeightHistogram, WeightDistribution
    int printPredictorParam = 100;

    // Number of iterations before evaluating weights (0 = don't evaluate)
    public int evaluateIters = 0;

    boolean useOneVsAll = false;
    boolean useInPlaceSGD = false;
    int SGDPasses = -1;
    int tuneSampleSize = -1;

    boolean useSum = false;
    double tolerance = 1e-4;
    int QNsize = 15;
    int prior = LogPrior.LogPriorType.QUADRATIC.ordinal();
    double sigma = 1.0;
    double epsilon = 0.01;
    double l1reg = 0.0;
    double svmCost = 1;
    double svmLightCost = 0.01;
    int svmLightVerbosity = 0;
    int crossValidationFolds = 0;
    String wekaClassifierType = "weka.classifiers.functions.SMO";
    String wekaClassifierOptions = "";

    int featureCountThreshold = 0;

    PredictorType predictorType = PredictorType.LINEAR;


    public Flags(Flags flags)
    {
      this(flags, null);
    }

    public Flags(Flags flags, String outfilePrefix)
    {
      this.serFormat = flags.serFormat;
      this.serFormatVersion = flags.serFormatVersion;
      this.trainFeatsFile = flags.trainFeatsFile;
      if (outfilePrefix != null && trainFeatsFile != null) {
        this.trainFeatsFile = outfilePrefix + trainFeatsFile;
      }
      this.mergeTrainFiles = flags.mergeTrainFiles;
      this.trainFile = flags.trainFile;
      this.trainFiles = flags.trainFiles;
      this.testFeatsFile = flags.testFeatsFile;
      if (outfilePrefix != null && testFeatsFile != null) {
        this.testFeatsFile = outfilePrefix + testFeatsFile;
      }
      this.testFile = flags.testFile;
      this.testOutFile = flags.testOutFile;
      if (outfilePrefix != null && testOutFile != null) {
        this.testOutFile = outfilePrefix + testOutFile;
      }
      this.loadFeatures = flags.loadFeatures;
      if (outfilePrefix != null && loadFeatures != null) {
        this.loadFeatures = outfilePrefix + loadFeatures;
      }
      this.saveFeatures = flags.saveFeatures;
      if (outfilePrefix != null && saveFeatures != null) {
        this.saveFeatures = outfilePrefix + saveFeatures;
      }
      this.loadPredictor = flags.loadPredictor;
      if (outfilePrefix != null && loadPredictor != null) {
        this.loadPredictor = outfilePrefix + loadPredictor;
      }
      this.savePredictor = flags.savePredictor;
      if (outfilePrefix != null && savePredictor != null) {
        this.savePredictor = outfilePrefix + savePredictor;
      }

      this.printPredictorFile = flags.printPredictorFile;
      if (outfilePrefix != null && printPredictorFile != null) {
        this.printPredictorFile = outfilePrefix + printPredictorFile;
      }
      this.printPredictorInfo = flags.printPredictorInfo;
      this.printPredictorParam = flags.printPredictorParam;

      this.useProbabilityAsScore = flags.useProbabilityAsScore;
      this.evaluateIters = flags.evaluateIters;
      this.useOneVsAll = flags.useOneVsAll;
      this.useInPlaceSGD = flags.useInPlaceSGD;
      this.SGDPasses = flags.SGDPasses;
      this.tuneSampleSize = flags.tuneSampleSize;

      this.useSum = flags.useSum;
      this.tolerance = flags.tolerance;
      this.QNsize = flags.QNsize;
      this.prior = flags.prior;
      this.sigma = flags.sigma;
      this.epsilon = flags.epsilon;
      this.l1reg = flags.l1reg;
      this.svmCost = flags.svmCost;
      this.svmLightCost = flags.svmLightCost;
      this.svmLightVerbosity = flags.svmLightVerbosity;
      this.crossValidationFolds = flags.crossValidationFolds;
      this.predictorType = flags.predictorType;

      this.deleteTrainFile = flags.deleteTrainFile;

      this.wekaClassifierType = flags.wekaClassifierType;
      this.wekaClassifierOptions = flags.wekaClassifierOptions;

      this.featureCountThreshold = flags.featureCountThreshold;
    }

    public Flags(Properties props) {
      this(props, null);
    }

    public Flags(Properties props, String prefix) {
      trainFeatsFile = Util.getProperty(props, prefix, "trainFeatsFile");
      trainFile = Util.getProperty(props, prefix, "trainFile");
      mergeTrainFiles = Boolean.parseBoolean(Util.getProperty(props, prefix, "mergeTrainFiles",
              String.valueOf(mergeTrainFiles)));
      if (trainFile != null && mergeTrainFiles) {
        trainFiles = trainFile.split("[,\\s]+");
        System.err.println("Got train files: " + trainFiles.length);
      }
      testFeatsFile = Util.getProperty(props, prefix, "testFeatsFile");
      testFile = Util.getProperty(props, prefix, "testFile");
      testOutFile = Util.getProperty(props, prefix, "testOutFile");
      loadFeatures = Util.getProperty(props, prefix, "loadFeatures");
      saveFeatures = Util.getProperty(props, prefix, "saveFeatures");
      loadPredictor = Util.getProperty(props, prefix, "loadPredictor");
      savePredictor = Util.getProperty(props, prefix, "savePredictor");

      printPredictorFile = Util.getProperty(props, prefix, "printPredictorFile");
      printPredictorInfo = Util.getProperty(props, prefix, "printPredictorInfo", printPredictorInfo);
      printPredictorParam = Integer.parseInt(Util.getProperty(props, prefix, "printPredictorParam",
              String.valueOf(printPredictorParam)));

      predictorType = predictorType.valueOf(Util.getProperty(props, prefix, "predictorType", predictorType.name()));
      useOneVsAll = Boolean.parseBoolean(Util.getProperty(props, prefix, "useOneVsAll",
              String.valueOf(useOneVsAll)));
      evaluateIters = Integer.parseInt(Util.getProperty(props, prefix, "evaluateIters",
              String.valueOf(evaluateIters)));
      useInPlaceSGD = Boolean.parseBoolean(Util.getProperty(props, prefix, "useInPlaceSGD",
              String.valueOf(useInPlaceSGD)));
      SGDPasses = Integer.parseInt(Util.getProperty(props, prefix, "SGDPasses",
              String.valueOf(SGDPasses)));
      tuneSampleSize = Integer.parseInt(Util.getProperty(props, prefix, "tuneSampleSize",
              String.valueOf(tuneSampleSize)));
      l1reg = Double.parseDouble(Util.getProperty(props, prefix, "l1reg",
              String.valueOf(l1reg)));
      sigma = Double.parseDouble(Util.getProperty(props, prefix, "sigma",
              String.valueOf(sigma)));
      svmCost = Double.parseDouble(Util.getProperty(props, prefix, "svmCost",
              String.valueOf(svmCost)));
      svmLightCost = Double.parseDouble(Util.getProperty(props, prefix, "svmLightCost",
              String.valueOf(svmLightCost)));
      svmLightVerbosity = Integer.parseInt(Util.getProperty(props, prefix, "svmLightVerbosity",
              String.valueOf(svmLightVerbosity)));
      crossValidationFolds = Integer.parseInt(Util.getProperty(props, prefix, "crossValidationFolds",
              String.valueOf(crossValidationFolds)));
      serFormatVersion =  Integer.parseInt(Util.getProperty(props, prefix, "serFormatVersion",
              String.valueOf(serFormatVersion)));
      serFormat =  TrainedPredictor.SerializationFormat.valueOf(Util.getProperty(props, prefix, "serFormat",
              String.valueOf(serFormat)));
      useProbabilityAsScore = Boolean.valueOf(Util.getProperty(props, prefix, "useProbabilityAsScore", String.valueOf(useProbabilityAsScore)));

      wekaClassifierType = Util.getProperty(props, prefix, "weka.classifier");
      wekaClassifierOptions = Util.getProperty(props, prefix, "weka.options");

      featureCountThreshold = Integer.valueOf(Util.getProperty(props, prefix, "featureCountThreshold", String.valueOf(featureCountThreshold)));
    }

    public String[] getTrainFilenames()
    {
      return (trainFiles != null)? trainFiles: (trainFile != null)? new String[] { trainFile }: new String[]{};
    }

    public String[] getTestFilenames()
    {
      return (testFile != null)? new String[] { testFile }: new String[]{};
    }

    public String[] getTrainTestFilenames()
    {
      String[] trainFilenames = getTrainFilenames();
      String[] testFilenames = getTestFilenames();
      if (testFilenames == null || testFilenames.length == 0) { return trainFilenames; }
      if (trainFilenames == null || trainFilenames.length == 0) { return testFilenames; }
      String[] combined = new String[trainFilenames.length + testFilenames.length];
      System.arraycopy(trainFilenames, 0, combined, 0, trainFilenames.length);
      System.arraycopy(testFilenames, 0, combined, trainFilenames.length, testFilenames.length);
      return combined;
    }

  }

  public void init(Properties props)
  {
    flags = new Flags(props);
  }

  public void init(Properties props, String prefix)
  {
    flags = new Flags(props, prefix);
  }

  public void init(Flags flags)
  {
    this.flags = flags;
  }

  public void prepareDatasets()
  {
    if (flags.trainFile != null && flags.trainFile.length() > 0) {
      trainData = new FileBasedDatasetInfo(flags.trainFile, flags.trainFiles, flags.trainFeatsFile);
    }
    if (flags.testFile != null && flags.testFile.length() > 0) {
      testData = new FileBasedDatasetInfo(flags.testFile, flags.testFeatsFile);
    }
    predictor.createDatasets(trainData, testData);
  }

  public void setupPredictor()
  {
    if (flags.loadPredictor != null) {
      // Load predictor
      try {
        predictor.loadPredictor(flags.loadPredictor, flags.serFormat, flags.serFormatVersion);
      } catch (Exception ex) {
        throw new RuntimeException(ex);
      }
    } else {
      // Train predictor
      trainPredictor();
      if (flags.savePredictor != null) {
        try {
          predictor.savePredictor(flags.savePredictor, flags.serFormat, flags.serFormatVersion);
        } catch (Exception ex) {
          throw new RuntimeException(ex);
        }
      }
    }
  }

  public void trainPredictor()
  {
    // TODO: Applying feature count here changes the dataset (should we make a copy?)
    trainData.dataset.applyFeatureCountThreshold(flags.featureCountThreshold);
    trainPredictor(trainData.dataset);
    predictor.stats = predictor.calcPredictorStats(trainData);
    double score = predictor.score("TrainSet", trainData.dataset);
    predictor.stats.setScore(score);
    System.err.println(predictor.stats);
  }

  abstract public void trainPredictor(GeneralDataset<L,F> trainingData);

  public void printPredictor()
  {
    if (flags.printPredictorInfo != null) {
      if (flags.printPredictorFile != null) {
        System.err.println("Predictor description at: " + flags.printPredictorFile);
        predictor.printPredictorDescription(flags.printPredictorFile, flags.printPredictorInfo, flags.printPredictorParam);
      } else {
        System.err.println("Predictor description:");
        String str = predictor.getPredictorDescription(flags.printPredictorInfo, flags.printPredictorParam);
        System.err.println(str);
      }
    }
  }

  public void evaluateTestset() {
    if (testData != null) {
      predictor.predictAndPrint("TestSet" + testData.getName(), testData, flags.testOutFile, true);
    }

  }

  public void cleanup()
  {
    // Do any special cleanup
    predictor.unloadPredictor();  // Free up memory
    if (flags.deleteTrainFile) {
      System.err.println("Deleting train file: " + flags.trainFile);
      File f = new File(flags.trainFile);
      f.delete();
    }
  }

}
