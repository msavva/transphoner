package org.babysherlock.classify;

import edu.stanford.nlp.classify.GeneralDataset;
import edu.stanford.nlp.util.Pair;

import java.util.List;

/**
 * Dataset that is stored on file
 *
 * @author Angel Chang
 */
public class FileBasedDatasetInfo<E,L,F> extends DatasetInfo<E,L,F> {
  String filename;
  String[] filenames;  // If multiple files combine to form this dataset
  String featsFilename;

  public FileBasedDatasetInfo(String filename, String featsFilename)
  {
    this.filename = filename;
    this.featsFilename = featsFilename;
    this.name = filename;
  }

  public FileBasedDatasetInfo(String filename, String[] filenames, String featsFilename)
  {
    this(filename, featsFilename);
  }

  public String[] getFilenames()
  {
    return (filenames != null)? this.filenames:new String[] { filename };
  }

  public FileBasedDatasetInfo(String filename, String featsFilename,
                              List<Pair<E, L>> items, GeneralDataset<L, F> dataset)
  {
    this(filename, featsFilename);
    this.items = items;
    this.dataset = dataset;
  }
}
