package org.babysherlock.classify;

import edu.stanford.nlp.classify.GeneralDataset;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.util.Function;
import edu.stanford.nlp.util.Pair;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

/**
 * Represents a dataset along with metadata bout the dataset
 *
 * @author Angel Chang
 */
public class DatasetInfo<E,L,F> {
  public GeneralDataset<L,F> dataset;
  public List<Pair<E,L>> items;
  public String name;
  public Function<E,String> itemToInstanceId = new Function<E,String>() {
    public String apply(E in) {
      return in.toString();
    }
  };

  public String getName() {
    return name;
  }

  public int size() {
    return dataset.size();
  }

  public Datum<L,F> getDatum(int i) {
    return dataset.getDatum(i);
  }

  public L getLabel(int i) {
    return dataset.getDatum(i).label();
  }

  public E getInstance(int i) {
    return items.get(i).first();
  }

  public String getInstanceId(int i) {
    return itemToInstanceId.apply(items.get(i).first());
  }

  public void print(PrintWriter pw)
  {
    for (int i = 0; i < size(); i++) {
      E instance = getInstance(i);
      Datum<L,F> datum = getDatum(i);
      String id = itemToInstanceId.apply(instance);
      pw.println(id + "\t" + datum.label());
    }
    pw.flush();
  }

  public void save(String filename) throws IOException
  {
    PrintWriter pw = IOUtils.getPrintWriter(filename);
    print(pw);
    pw.close();
  }

}
