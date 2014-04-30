package org.babysherlock.classify;

import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.stats.Counter;

import java.io.IOException;

/**
 * Trained predictor with a feature factory
 *
 * @author Angel Chang
 */
abstract public class TrainedPredictorWithFeatureFactory<P,S extends PredictorStats<L>,E,L,F>
        extends TrainedPredictor<P,S,E,L,F> {
  FeatureFactory<E,L,F> featureFactory;

  public L valueOf(E item) {
    Datum<L,F> datum = featureFactory.getDatum(item);
    return valueOf(datum);
  }

  public FeatureFactory<E,L,F> getFeatureFactory() {
    return featureFactory;
  }

  public void setFeatureFactory(FeatureFactory<E,L,F> featureFactory) {
    this.featureFactory = featureFactory;
  }

  Counter<F> getFeatures(E item) {
    return featureFactory.getFeatures(item);
  }

  public void saveDatasetFeats(DatasetInfo<E,L,F> dataInfo, String featsFilename)
  {
    if (featsFilename != null) {
      try {
        featureFactory.printLabelledFeats(featsFilename, dataInfo.items);
      } catch (IOException ex) {
        System.err.println("Error saving features to " + featsFilename);
        ex.printStackTrace(System.err);
      }
    }
  }

  public void createDatasets(DatasetInfo<E,L,F>... dataInfos)
  {
    for (DatasetInfo<E,L,F> dataInfo:dataInfos) {
      if (dataInfo != null) {
        if (dataInfo instanceof FileBasedDatasetInfo) {
          // TODO: Make actually work...
          FileBasedDatasetInfo<E,L,F> f = (FileBasedDatasetInfo<E,L,F>) dataInfo;
          if (f.filenames != null) {
            throw new UnsupportedOperationException("Multiple training files not supported");
          }
          dataInfo.dataset = featureFactory.getLabeledDataset(dataInfo.items, f.filename);
          saveDatasetFeats(f, f.featsFilename);
        }
      }
    }
  }

}
