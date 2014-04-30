package org.babysherlock.classify;

import com.cedarsoftware.util.io.JsonReader;
import com.cedarsoftware.util.io.JsonWriter;
import edu.stanford.nlp.classify.*;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.util.CacheMap;
import edu.stanford.nlp.util.ErasureUtils;
import edu.stanford.nlp.util.Function;
import edu.stanford.nlp.util.StringUtils;
import org.yaml.snakeyaml.Yaml;

import java.io.*;
import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Trained predictor
 *
 * @author Angel Chang
 */
public abstract class TrainedPredictor<P,S extends PredictorStats<L>,E,L,F> implements Predictor<L,F> {
  P predictor;
  String predictorId;
  S stats;
  Flags flags;

  // Serialization formats: Only BINARY/JSON currently working (don't use YAML for now)
  public enum SerializationFormat {
    BINARY("ser"),
    YAML("yml"),
    JSON("json");

    public String extension;
    SerializationFormat(String extension) {
      this.extension = extension;
    }
  }

  abstract public S calcPredictorStats(DatasetInfo<E,L,F> trainData);
  abstract public void createDatasets(DatasetInfo<E,L,F>... dataInfos);
  abstract public L valueOf(Datum<L,F> datum);

  public static class Flags {
    String predictorFile;
    boolean useProbabilityAsScore;
    SerializationFormat serFormat = SerializationFormat.BINARY;

    public Flags(String predictorFile, boolean useProbabilityAsScore) {
      this.predictorFile = predictorFile;
      this.useProbabilityAsScore = true;
    }
    public Flags(Properties props, String prefix) {
      predictorFile = Util.getProperty(props, prefix, "loadPredictor");
      useProbabilityAsScore = Boolean.valueOf(Util.getProperty(props, prefix, "useProbabilityAsScore", String.valueOf(useProbabilityAsScore)));
      serFormat =  TrainedPredictor.SerializationFormat.valueOf(Util.getProperty(props, prefix, "serFormat",
              String.valueOf(serFormat)));
    }
  }

  public static class PredictorCacheMap<K, V extends TrainedPredictor> extends CacheMap<K, V>
  {
    public PredictorCacheMap(int numEntries) {
      super(numEntries);
    }

    @Override
    protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
      boolean removed = super.removeEldestEntry(eldest);
      if (removed) {
        eldest.getValue().unloadPredictor();
      }
      return removed;
    }
  }


  public void init(Properties props, String prefix)
  {
    flags = new Flags(props, prefix);
  }

  public void init(Flags flags) {
    this.flags = flags;
  }

  public boolean isPredictorLoaded() {
    return predictor != null;
  }

  public void unloadPredictor() {
    predictor = null;
  }

  public void loadPredictor() {
    try {
      loadPredictor(flags.predictorFile, flags.serFormat, 1);
    } catch (Exception ex) {
      throw new RuntimeException("Error loading predictor from " + flags.predictorFile, ex);
    }
  }

  public P getPredictor(boolean load)
  {
    if (load && !isPredictorLoaded()) {
      loadPredictor();
    }
    return predictor;
  }

  public P getPredictor()
  {
    return predictor;
  }

  public String getPredictorId() {
    return predictorId;
  }

  public void setPredictorId(String predictorId) {
    this.predictorId = predictorId;
  }

  public S getPredictorStats()
  {
    return stats;
  }

  public void savePredictor(String filename, SerializationFormat format, int serFormatVersion) throws IOException
  {
    System.err.println("Saving predictor to " + filename + " using format " + format);
    Map<String,String> customFields = new HashMap<String,String>();
    saveCustomFields(customFields);
    switch (format) {
      case BINARY:
        ObjectOutputStream os = new ObjectOutputStream(new BufferedOutputStream(IOUtils.getFileOutputStream(filename)));
        //      os.writeObject(flags);
        os.writeInt(serFormatVersion);
        os.writeObject(predictorId);
        os.writeObject(stats);
        os.writeObject(predictor);
        if (customFields != null && !customFields.isEmpty()) os.writeObject(customFields);
        os.close();
      break;
      case YAML:
      case JSON:
        PrintWriter pw = IOUtils.getPrintWriter(filename);
        Map<String, Object> savedInfo = new HashMap<String,Object>();
        savedInfo.put("version", serFormatVersion);
        savedInfo.put("predictorId", predictorId);
        savedInfo.put("predictor", predictor);
        if (customFields != null && !customFields.isEmpty()) {
          savedInfo.put("fields", customFields);
        }
        if (format == SerializationFormat.YAML) {
          Yaml yaml = new Yaml();
          // NOTE: snakeyaml only handles bean like objects so the actual predictor is not saved correctly
          // need to find better yaml serializer...
          yaml.dump(savedInfo, pw);
        } else {
          // NOTE: Use json-io since it saves the class type
          //  (can also use gson to the convert the object, nicer format, but need to explicitly save the types)
          // JSON
          String json = JsonWriter.objectToJson(savedInfo);
          pw.println(json);
        }
        pw.close();
      break;
      default: throw new UnsupportedOperationException("Unsupported serialization format: " + format);
    }
    System.err.println("Predictor saved at " + filename);
  }

  public void loadPredictor(String filename, SerializationFormat format, int serFormatVersion) throws IOException, ClassNotFoundException
  {
    File file = new File(filename);
    if (!file.exists()) {
      System.err.println("Cannot load predictor: " + filename + " does not exist");
      return;
    }
    loadPredictorVersion(filename, format, serFormatVersion);
  }

  public void loadPredictorVersion(String filename, SerializationFormat format, int version) throws IOException, ClassNotFoundException
  {
    System.err.println("Loading predictor from " + filename + ", version " + version + ", format " + format);
    Map<String,String> customFields = null;
    switch (format) {
      case BINARY:
        ObjectInputStream ois = new ObjectInputStream(new BufferedInputStream(IOUtils.getFileInputStream(filename)));
        //      flags = ErasureUtils.<String> uncheckedCast(ois.readObject());
        int serVersion = ois.readInt();
        predictorId = ErasureUtils.uncheckedCast(ois.readObject());
        stats = ErasureUtils.uncheckedCast(ois.readObject());
        predictor = ErasureUtils.uncheckedCast(ois.readObject());
        // Try to read custom fields
        try {
          customFields = ErasureUtils.uncheckedCast(ois.readObject());
        } catch (Exception ex) {
          customFields = null;
        }
        ois.close();
      break;
      case YAML:
      case JSON:
        Map<String,Object> savedInfo = null;
        if (format == SerializationFormat.YAML) {
          BufferedReader br = IOUtils.getBufferedFileReader(filename);
          Yaml yaml = new Yaml();
          savedInfo = ErasureUtils.uncheckedCast(yaml.load(br));
          br.close();
        } else {
          // JSON
          String str = IOUtils.slurpFile(filename);
          savedInfo = ErasureUtils.uncheckedCast(JsonReader.toJava(str));
        }
        predictorId = ErasureUtils.uncheckedCast(savedInfo.get("predictorId"));
        predictor = ErasureUtils.uncheckedCast(savedInfo.get("predictor"));
        customFields = ErasureUtils.uncheckedCast(savedInfo.get("fields"));
       break;
      default: throw new UnsupportedOperationException("Unsupported serialization format: " + format);
    }
    if (customFields != null) {
      loadCustomFields(customFields);
    }
    System.err.println("Predictor loaded from " + filename);
  }

  // Overload to save custom fields
  protected void saveCustomFields(Map<String,String> customFields) {}

  // Overload to load custom fields
  protected void loadCustomFields(Map<String,String> customFields) {}

  public void printPredictorDescription(String filename, String style, int param)
  {
    try {
      PrintWriter pw = IOUtils.getPrintWriter(filename);
      String str = getPredictorDescription(style, param);
      pw.println(str);
      pw.close();
    } catch (IOException ex) {
      throw new RuntimeException(ex);
    }
  }

  public String getPredictorDescription(String style, int param)
  {
    return predictor.toString();
  }

  // Scores the predictor
  public double score(String datasetName, GeneralDataset<L, F> dataset)
  {
    PredictorScorer scorer = getScorer();
    double score = scorer.score(this, dataset);
    System.err.println(formatScore(datasetName + ": " + scorer.getName(), score, 3));
    return score;
  }

  public PredictorScorer getScorer()
  {
    PredictorScorer scorer = new PredictorScorer.MeanErrorSq();
    return scorer;
  }


  public static String formatScore(String name, double score, int numDigits) {
    NumberFormat nf = NumberFormat.getNumberInstance();
    nf.setMaximumFractionDigits(numDigits);
    return name + " = " + nf.format(score);
  }

  public void predictAndPrint(DatasetInfo<E,L,F> dataInfo, PrintWriter pw)
  {
    for (int i = 0; i < dataInfo.size(); i++) {
      E instance = dataInfo.getInstance(i);
      String id = dataInfo.itemToInstanceId.apply(instance);
      Datum<L,F> datum = dataInfo.getDatum(i);
      L predict = valueOf(datum);
      pw.println(id + "\t" + datum.label() + "\t" + predict );
    }
    pw.flush();
  }

  public double predictAndPrint(String name, DatasetInfo<E,L,F> data, String outputFile, boolean doScore)
  {
    double score = (doScore)? score(name, data.dataset):0;
    if (outputFile != null) {
      try {
        PrintWriter pw = IOUtils.getPrintWriter(outputFile);
        predictAndPrint(data, pw);
        pw.close();
      } catch (IOException ex) {
        System.err.println("Error opening test output " + outputFile);
      }
    } else {
      predictAndPrint(data, new PrintWriter(System.out));
    }
    return score;
  }

  public void predictAndPrintTsv(DatasetInfo<E,L,F> dataInfo, PrintWriter pw, Function<E,String[]> itemToFields, String delimiter)
  {
    for (int i = 0; i < dataInfo.size(); i++) {
      E instance = dataInfo.getInstance(i);
      String[] fields = itemToFields.apply(instance);
      Datum<L,F> datum = dataInfo.getDatum(i);
      L predict = valueOf(datum);
      pw.println(StringUtils.join(fields, delimiter) + delimiter + datum.label() + delimiter + predict );
    }
    pw.flush();
  }

  public double predictAndPrintTsv(String name, DatasetInfo<E,L,F> data, String outputFile, boolean doScore,
                                   Function<E,String[]> itemToFields, String[] fields, String delimiter)
  {
    double score = (doScore)? score(name, data.dataset):0;
    if (outputFile != null) {
      try {
        PrintWriter pw = IOUtils.getPrintWriter(outputFile);
        pw.println(StringUtils.join(fields, delimiter) + delimiter + "label" + delimiter + "predicted");
        predictAndPrintTsv(data, pw, itemToFields, delimiter);
        pw.close();
      } catch (IOException ex) {
        System.err.println("Error opening test output " + outputFile);
      }
    } else {
      predictAndPrint(data, new PrintWriter(System.out));
    }
    return score;
  }

}
