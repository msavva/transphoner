package org.babysherlock.classify;

import edu.stanford.nlp.classify.GeneralDataset;

import java.io.Serializable;
import java.util.*;

/**
 * Statistics for a predictor
 *
 * @author Angel Chang
 */
public class PredictorStats<L> implements Serializable {
  private static final long serialVersionUID = 1;

  String predictorId;
  int trainingSize = 0;
  double score;
  // Additional stats
  Map<String,Object> statsMap = new LinkedHashMap<String,Object>();

  public PredictorStats(String predictorId)
  {
    this.predictorId = predictorId;
  }

  public <F> PredictorStats(String predictorId, GeneralDataset<L, F> dataset)
  {
    this.predictorId = predictorId;
    this.trainingSize = dataset.size();
  }

  public void setScore(double s) {
    score = s;
  }

  public double getScore() {
    return score;
  }

  public String toString()
  {
    StringBuilder sb = new StringBuilder();
    sb.append("Predictor " + predictorId);
    sb.append("\tscore=").append(getScore());
    return sb.toString();
  }

  public Iterable<String> getStatKeys() {
    return statsMap.keySet();
  }

  public Object getStat(String key) {
    return statsMap.get(key);
  }

  public void setStats(String key, Object val) {
    statsMap.put(key, val);
  }

  public List<String> getFieldNames() {
    List<String> list = new ArrayList<String>();
    list.add("predictorId");
    list.add("score");
    return list;
  }

  public List<Object> getFields(Iterable<String> additionalStats) {
    List<Object> list = new ArrayList<Object>();
    list.add(getScore());
    for (String k: additionalStats) {
      Object v = statsMap.get(k);
      list.add(v);
    }
    return list;
  }
}

