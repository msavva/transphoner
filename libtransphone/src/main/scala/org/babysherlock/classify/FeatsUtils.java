package org.babysherlock.classify;

import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.stats.Counters;
import edu.stanford.nlp.stats.TwoDimensionalCounter;

import java.io.PrintWriter;
import java.util.*;

/**
 * Utility functions for working with features as counters
 *
 * @author Angel Chang
 */
public class FeatsUtils {
  protected static String COMP_FEAT_SEPARATOR = ":";
  protected static String COMP_LABEL_SEPARATOR = ".";

  protected static Counter<String> toCounter(Map<String, TwoDimensionalCounter<String,String>> map)
  {
    if (map == null) return null;
    Counter<String> counter = new ClassicCounter<String>();
    for (String component:map.keySet()) {
      TwoDimensionalCounter<String,String> compCounter = map.get(component);
      for (String label:compCounter.firstKeySet()) {
        Counter<String> labelCounter = compCounter.getCounter(label);
        for (String feat:labelCounter.keySet()) {
          String prefix = (feat.length() > 0)? component + COMP_FEAT_SEPARATOR + feat:component;
          String countFeatName = label;
          if (prefix.length() > 0) {
            countFeatName = prefix + COMP_LABEL_SEPARATOR + label;
          }
          counter.incrementCount(countFeatName, labelCounter.getCount(feat));
        }
      }
    }
    return counter;
  }

  protected static Counter<String> mergeFeatureCounts(Counter<String> counts1, Counter<String> counts2)
  {
    return mergeFeatureCounts(null, counts1, null, counts2);
  }

  protected static Counter<String> mergeFeatureCounts(String prefix1, Counter<String> counts1, String prefix2, Counter<String> counts2)
  {
    if (counts1 == null && counts2 == null) { return null; }
    Counter<String> mergedFeatures = (counts1 != null)? counts1.getFactory().create(): counts2.getFactory().create();
    addFeatureCounts(mergedFeatures, counts1, prefix1);
    addFeatureCounts(mergedFeatures, counts2, prefix2);
    return mergedFeatures;
  }

  protected static void addFeatureCounts(Counter<String> features, Counter<String> counts, String prefix)
  {
    if (counts == null) return;
    if (prefix == null) {
      features.addAll(counts);
    } else {
      for (String f:counts.keySet()) {
        features.incrementCount(prefix + f, counts.getCount(f));
      }
    }
  }

  /** Functions for printing features/label scores **/

  public static <F> void  printBinaryFeats(PrintWriter pw, String instanceHeader, Counter<F> feats) {
    printBinaryFeats(pw, null, instanceHeader, feats, null);
  }

  public static <F> void  printBinaryFeats(PrintWriter pw, String instanceHeader, Counter<F> feats, Comparator<F> comparator) {
    printBinaryFeats(pw, null, instanceHeader, feats, comparator);
  }

  public static <F> void printBinaryFeats(PrintWriter pw, String prefix, String instanceHeader, Collection<F> feats, Comparator<F> comparator) {
    pw.println(instanceHeader);
    if (feats != null) {
      if (comparator != null) {
        List<F> sorted = new ArrayList<F>(feats);
        Collections.sort(sorted, comparator);
        feats = sorted;
      }
      if (prefix != null) {
       for (F f:feats) {
          pw.println(prefix + COMP_LABEL_SEPARATOR + f);
       }
      } else {
        for (F f:feats) {
          pw.println(f);
        }
      }
    }
    pw.println();
    pw.flush();
  }

  public static <F> void printBinaryFeats(PrintWriter pw, String prefix, String instanceHeader, Counter<F> feats, Comparator<F> comparator) {
    Collection<F> keys = feats.keySet();
    printBinaryFeats(pw, prefix, instanceHeader, keys, comparator);
  }

  public static <F> void  printFeats(PrintWriter pw, String instanceHeader, Counter<F> feats) {
    printFeats(pw, null, instanceHeader, feats);
  }

  public static <F> void printFeats(PrintWriter pw, String prefix, String instanceHeader, Counter<F> feats) {
    pw.println(instanceHeader);
    if (feats != null) {
      String format = (prefix != null)? prefix + COMP_LABEL_SEPARATOR + "%s\t%g":"%s\t%g";
      String str = Counters.toVerticalString(feats, Integer.MAX_VALUE, format, true);
      pw.println(str);
    }
    pw.println();
    pw.flush();
  }

  public static String getInstanceHeader(String instanceId, String label, String desc)
  {
    StringBuilder sb = new StringBuilder("<");
    sb.append(instanceId);
    if (label != null) {
      sb.append("#").append(label);      
    }
    sb.append(">");
    if (desc != null) {
      sb.append("\t").append(desc);
    }
    return sb.toString();
  }

}
