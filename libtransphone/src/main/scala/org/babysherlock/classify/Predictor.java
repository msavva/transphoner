package org.babysherlock.classify;

import edu.stanford.nlp.ling.Datum;

/**
 * Simple predictor
 *
 * @author Angel Chang
 */
public interface Predictor<L,F> {
  L valueOf(Datum<L,F> datum);
}
