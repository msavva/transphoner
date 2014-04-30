package org.babysherlock.classify;

import edu.stanford.nlp.classify.GeneralDataset;
import edu.stanford.nlp.classify.RVFDataset;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.ling.RVFDatum;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.util.Pair;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

/**
 * Creates datums from other object
 *
 * @author Angel Chang
 */
public abstract class FeatureFactory<E,L extends Object,F>  {
  Comparator<F> comparator;

  abstract public Counter<F> getFeatures(E data);

  public void setProperties(Properties props, String prefix) {
  }

  public Collection<Datum<L,F>> getDatumGroup(E data)
  {
    Datum<L,F> datum = getDatum(data);
    if (datum != null) {
      Collection<Datum<L,F>> list = new ArrayList<Datum<L,F>>(1);
      list.add(datum);
      return list;
    }
    return null;
  }

  public Datum<L,F> getDatum(E data)
  {
    Counter<F> features = getFeatures(data);
    return (features != null)? new RVFDatum<L,F>(features):null;
  }

  public Datum<L,F> getDatum(E data, L label)
  {
    Counter<F> features = getFeatures(data);
    return (features != null)? new RVFDatum<L,F>(features, label):null;
  }

  public GeneralDataset<L,F> getDataset(List<E> items, String source)
  {
    int skipped = 0;
    RVFDataset<L,F> dataset = new RVFDataset<L,F>();
    for (E item:items) {
      Datum<L,F> datum = getDatum(item);
      if (datum != null) {
        // TODO: we really just want to store extra info with the datum
        if (item instanceof HasInstanceId) {
          dataset.add(datum, source, ((HasInstanceId) item).getInstanceId());
        } else {
          dataset.add(datum, source, null);
        }
      } else {
        skipped++;
      }
    }
    if (skipped > 0) {
      System.err.println("WARNING: " + skipped + "/" + items.size() + " datums skipped (no features)");
    }
    return dataset;
  }

  public GeneralDataset<L,F> getLabeledDataset(List<Pair<E,L>> labeledItems, String source)
  {
    int skipped = 0;
    RVFDataset<L,F> dataset = new RVFDataset<L,F>();
    for (Pair<E,L> labeledItem:labeledItems) {
      Datum<L,F> datum = getDatum(labeledItem.first(), labeledItem.second());
      if (datum != null) {
        // TODO: we really just want to store extra info with the datum
        if (labeledItem.first() instanceof HasInstanceId) {
          dataset.add(datum, source, ((HasInstanceId) labeledItem.first()).getInstanceId());
        } else {
          dataset.add(datum, source, null);
        }
      } else {
        skipped++;
      }
    }
    if (skipped > 0) {
      System.err.println("WARNING: " + skipped + "/" + labeledItems.size() + " datums skipped (no features)");
    }
    return dataset;
  }

  /** Functions for printing features/label scores **/
  abstract public String getInstanceHeader(E item, L label);

  public void printFeats(PrintWriter pw, E item, L label, Counter<F> feats)
  {
    FeatsUtils.printFeats(pw, getInstanceHeader(item, label), feats);
  }

  public void printBinaryFeats(PrintWriter pw, E item, L label, Counter<F> feats)
  {
    FeatsUtils.printBinaryFeats(pw, getInstanceHeader(item, label), feats, comparator);
  }

  public void printFeats(String filename, Iterable<E> items) throws IOException {
    PrintWriter pw = IOUtils.getPrintWriter(filename);
    printFeats(pw, items);
    pw.close();
  }

  public void printFeats(PrintWriter pw, Iterable<E> items) throws IOException {
    int skipped = 0;
    int total = 0;
    for (E item:items) {
      Counter<F> features = getFeatures(item);
      if (features != null) {
        printFeats(pw, item, null, features);
      } else {
        skipped++;
      }
      total++;
    }
    if (skipped > 0) {
      System.err.println("WARNING: " + skipped + "/" + total + " datums skipped (no features)");
    }
  }

  public void printLabelledFeats(String filename, List<Pair<E,L>> labeledItems) throws IOException {
    PrintWriter pw = IOUtils.getPrintWriter(filename);
    printLabelledFeats(pw, labeledItems);
    pw.close();
  }

  public void printLabelledFeats(PrintWriter pw, List<Pair<E,L>> labeledItems) throws IOException {
    int skipped = 0;
    for (Pair<E,L> labeledItem:labeledItems) {
      Counter<F> features = getFeatures(labeledItem.first());
      if (features != null) {
        printFeats(pw, labeledItem.first(), labeledItem.second(), features);
      } else {
        skipped++;
      }
    }
    if (skipped > 0) {
      System.err.println("WARNING: " + skipped + "/" + labeledItems.size() + " datums skipped (no features)");
    }
  }

  public static interface HasInstanceId
  {
    public String getInstanceId();
  }

  public static interface HasLabel<L>
  {
    public L getLabel();
  }
}
