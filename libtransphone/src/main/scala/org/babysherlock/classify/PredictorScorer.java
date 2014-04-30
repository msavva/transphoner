package org.babysherlock.classify;

import edu.stanford.nlp.classify.GeneralDataset;
import edu.stanford.nlp.ling.Datum;

/**
 * Scorer for regressions
 *
 * @author Angel Chang
 */
public interface PredictorScorer<L> {
  public <F> double score(Predictor<L,F> predictor, GeneralDataset<L,F> data) ;
  public String getName();

  // Average of the error squared
  public static class MeanErrorSq implements PredictorScorer<Double> {
    @Override
    public <F> double score(Predictor<Double,F> classifier, GeneralDataset<Double, F> data) {
      double sum = 0;
      for (Datum<Double, F> d: data) {
        double predicted = classifier.valueOf(d);
        double correct = d.label();
        double diff = predicted - correct;
        sum += diff*diff;
      }
      return sum/data.size();
    }

    public String getName() {
      return "MeanErrorSq";
    }
  }
}
