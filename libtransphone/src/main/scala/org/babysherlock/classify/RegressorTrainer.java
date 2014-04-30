package org.babysherlock.classify;

import edu.stanford.nlp.classify.*;

/**
 * Trains a regressor
 *
 * @author Angel Chang
 */
public class RegressorTrainer<E,F, TP extends TrainedRegressorWithFeatureFactory<E,F>>
        extends PredictorTrainer<Regressor<F>, PredictorStats<Double>, E, Double, F, TP> {
  public RegressorTrainer(TP predictor)
  {
    super(predictor);
  }

  @Override
  public void trainPredictor(GeneralDataset<Double,F> trainingData)
  {
    switch (flags.predictorType) {
      case LINEAR:
        LinearRegressionFactory<F> lrf = new LinearRegressionFactory<F>();
        predictor.predictor = lrf.train(trainingData, flags.sigma, flags.tolerance);
        break;
      case SVMLIGHT:
        SVMLightRegressionFactory<F> svmFactory = new SVMLightRegressionFactory<F>();
        svmFactory.setCost(flags.svmLightCost);
//        svmFactory.setSvmLightVerbosity(flags.svmLightVerbosity);
//        svmFactory.setDeleteTempFilesOnExitFlag(true);
//        svmFactory.setDegree(flags.svmLightDegree);
//        svmFactory.setGamma(flags.svmLightGamma);
//        svmFactory.setKernelType(flags.svmLightKernelType);
//        if (flags.crossValidationFolds > 0) {
//          svmFactory.setTuneHeldOut(true);
//          svmFactory.setFolds(flags.crossValidationFolds);
//          svmFactory.setScorer(classifier.getScorer());
//        }
        predictor.predictor = svmFactory.train(trainingData);
        break;
      case WEKA:
      default:
        throw new UnsupportedOperationException("Unsupported predictorType " + flags.predictorType);
    }
  }

}