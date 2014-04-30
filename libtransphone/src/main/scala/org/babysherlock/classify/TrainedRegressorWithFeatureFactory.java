package org.babysherlock.classify;

import edu.stanford.nlp.classify.LinearRegressor;
import edu.stanford.nlp.classify.Regressor;
import edu.stanford.nlp.ling.Datum;

/**
 * trained regressor with a feature factory
 *
 * @author Angel Chang
 */
public class TrainedRegressorWithFeatureFactory<E,F>
        extends TrainedPredictorWithFeatureFactory<Regressor<F>,PredictorStats<Double>, E,Double,F> {
  @Override
  public Double valueOf(Datum<Double, F> datum) {
    return predictor.valueOf(datum);
  }

  public String getPredictorDescription(String style, int param)
  {
    if (predictor instanceof LinearRegressor) {
      LinearRegressor<F> regressor = (LinearRegressor<F>) predictor;
      return regressor.toString(style, param);
    } else {
      return predictor.toString();
    }
  }

  public PredictorStats<Double> calcPredictorStats(DatasetInfo<E,Double,F> trainData)
  {
    PredictorStats<Double> stats  = null;
    if (trainData != null) {
      stats = new PredictorStats<Double>(predictorId, trainData.dataset);
    }
    return stats;
  }
}
