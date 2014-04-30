package edu.stanford.nlp.classify;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.ling.RVFDatum;
import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.stats.Counters;
import edu.stanford.nlp.util.*;

/**
 * An L2 regularized linear regression model.
 * Use <code>LinearRegressionFactory</code> to train the model.
 * @author Ramesh (nmramesh@cs.stanford.edu)
 *
 * @param <F>
 */
public class LinearRegressor<F> implements Regressor<F>{
  /**
   * 
   */
  private static final long serialVersionUID = 8367022834638633057L;
  private double[] weights;
  private Index<F> featureIndex;
  
  public LinearRegressor(double[] weights, Index<F> featureIndex){
    this.weights = weights;
    this.featureIndex = featureIndex;
  }
  
  public double[] weights() {
    return weights;
  }
  public Index<F> featureIndex(){
    return featureIndex;
  }

  /**
   * 
   * @param datum
   * @return the regressed output value of the datum.
   */
  public double valueOf(Datum<Double,F> datum){
    if(datum instanceof RVFDatum)
      return valueOfRVFDatum((RVFDatum<Double,F>)datum);
    double output = 0;
    for(F feature : datum.asFeatures()){
      int fID = featureIndex.indexOf(feature);
      if(fID>=0)
        output += weights[fID];
    }
    return output;
  }

  /**
   * 
   * @param datum
   * @return a counter of weights for the features in the datum.
   */
  public Counter<F> justificationOf(Datum<Double,F> datum){
    if(datum instanceof RVFDatum)
      return justificationOfRVFDatum((RVFDatum<Double,F>)datum);
    Counter<F> featureWeights = new ClassicCounter <F>();
    for(F feature : datum.asFeatures()){
      int fID = featureIndex.indexOf(feature);
      if(fID>=0)
        featureWeights.incrementCount(feature, weights[fID]);
    }
    return featureWeights;
  }
  
  private Counter<F> justificationOfRVFDatum(RVFDatum<Double,F> datum){
    Counter<F> featureWeights = new ClassicCounter <F>();
    Counter<F> datumCounter = datum.asFeaturesCounter();
    for(F feature : datumCounter.keySet()){
      int fID = featureIndex.indexOf(feature);
      if(fID>=0){
        double weight = weights[fID]*datumCounter.getCount(feature);
        featureWeights.incrementCount(feature, weight);
      }
    }
    return featureWeights;
  }
  
  /**
   * 
   * @param numFeatures the number of top features to be returned.
   * @return a counter of top features with weights.
   */
  public Counter<F> getTopFeatures(int numFeatures){
    Counter<F> topWeights = getFeatureWeights();
    Counters.retainTop(topWeights,numFeatures);
    return topWeights;
  }

  
  public Counter<F> getFeatureWeights(){
    Counter<F> weightsCtr = new ClassicCounter<F>();
    for(int i = 0; i < featureIndex.size(); i++){
      F feature = featureIndex.get(i);
      weightsCtr.incrementCount(feature,weights[i]);
    }    
    return weightsCtr;
  }
  
  /**
   * 
   * @return a counter of all feature-weights
   */
  public Counter<F> getTopFeatures(){
    return getTopFeatures(featureIndex.size());
  }
  
  public List<Double> valuesOf(GeneralDataset<Double, F> data) {    
    List<Double> values = new ArrayList<Double>();
    for (int i = 0; i < data.size(); i++) {
      values.add(valueOf(data.getDatum(i)));
    }
    return values;
  }
  
  private double valueOfRVFDatum(RVFDatum<Double,F> datum){
    double output = 0;
    Counter<F> fCounter = datum.asFeaturesCounter();
    for(F feature : fCounter.keySet()){
      int fID = featureIndex.indexOf(feature);
      if(fID>=0)
        output += weights[fID]*fCounter.getCount(feature);
    }
    return output;
  }

  /**
   * Print out a partial representation of a linear regressor in one of
   * several ways.
   *
   * @param style Options are:
   *              HighWeight: print out the param parameters with largest weights;
   *              HighMagnitude: print out the param parameters for which the absolute
   *              value of their weight is largest;
   *
   * @param param Determines the number of things printed in certain styles
   * @throws IllegalArgumentException if the style name is unrecognized
   */
  public String toString(String style, int param) {
    if (style == null || "".equals(style)) {
      return "LinearRegressor with " + featureIndex.size() + " features\n";
    } else if (style.equalsIgnoreCase("HighWeight")) {
      return toBiggestWeightFeaturesString(false, param);
    } else if (style.equalsIgnoreCase("HighMagnitude")) {
      return toBiggestWeightFeaturesString(true, param);
    } else {
      throw new IllegalArgumentException("Unknown style: " + style);
    }
  }

  public String toString() {
    return toString("", 0);
  }

  /** Return a String that prints features with large weights.
   *
   * @param useMagnitude Whether the notion of "large" should ignore
   *                     the sign of the feature weight.
   * @param numFeatures  How many top features to print
   * @return The String representation of features with large weights
   */
  public String toBiggestWeightFeaturesString(boolean useMagnitude,
                                              int numFeatures) {
    List<Pair<F,Double>> topFeatures = getTopFeatures(useMagnitude, numFeatures);
    return featuresToString(topFeatures);
  }

  public int[] getTopFeatureIndices(boolean useMagnitude,
                                    int numFeatures) {
    edu.stanford.nlp.util.PriorityQueue<Integer> biggestKeys =
            new FixedPrioritiesPriorityQueue<Integer>();

    // locate biggest keys
    for (int feat = 0; feat < weights.length; feat++) {
      double thisWeight;
      // reverse the weight, so get smallest first
      if (useMagnitude) {
        thisWeight = -Math.abs(weights[feat]);
      } else {
        thisWeight = -weights[feat];
      }
      if (biggestKeys.size() == numFeatures) {
        // have enough features, add only if bigger
        double lowest = biggestKeys.getPriority();
        if (thisWeight < lowest) {
          // remove smallest
          biggestKeys.removeFirst();
          biggestKeys.add(feat, thisWeight);
        }
      } else {
        // always add it if don't have enough features yet
        biggestKeys.add(feat, thisWeight);
      }
    }

    int actualSize = biggestKeys.size();
    int[] topFeatures = new int[actualSize];
    for (int j = actualSize - 1; j >= 0; j--) {
      topFeatures[j] = biggestKeys.removeFirst();
    }
    return topFeatures;
  }

  public List<Pair<F,Double>> getTopFeatures(boolean useMagnitude,
                                             int numFeatures) {
    int[] featureIndices = getTopFeatureIndices(useMagnitude, numFeatures);
    List<Pair<F,Double>> topFeatures = new ArrayList<Pair<F,Double>>(featureIndices.length);
    for (int f: featureIndices) {
      topFeatures.add(Pair.makePair(featureIndex.get(f), weights[f]));
    }

    return topFeatures;
  }

  /** Return a String that prints features with large weights.
   *
   * @param topFeatures top features
   * @return The String representation of features with large weights
   */
  private String featuresToString(List<Pair<F,Double>> topFeatures) {
    // System.err.println("bigColl is " + bigColl);

    // find longest key length (for pretty printing) with a limit
    int maxLeng = 0;
    for (Pair<F,Double> p : topFeatures) {
      String key = "(" + p.first() + ")";
      int leng = key.length();
      if (leng > maxLeng) {
        maxLeng = leng;
      }
    }
    maxLeng = Math.min(64, maxLeng);

    // set up pretty printing of weights
    NumberFormat nf = NumberFormat.getNumberInstance();
    nf.setMinimumFractionDigits(4);
    nf.setMaximumFractionDigits(4);
    if (nf instanceof DecimalFormat) {
      ((DecimalFormat) nf).setPositivePrefix(" ");
    }

    //print high weight features to a String
    StringBuilder sb = new StringBuilder("LinearRegressor [printing top " + topFeatures.size() + " features]\n");
    for (Pair<F, Double> p : topFeatures) {
      String key = "(" + p.first() + ")";
      sb.append(StringUtils.pad(key, maxLeng));
      sb.append(" ");
      double cnt = p.second();
      if (Double.isInfinite(cnt)) {
        sb.append(cnt);
      } else {
        sb.append(nf.format(cnt));
      }
      sb.append("\n");
    }
    return sb.toString();
  }


}
