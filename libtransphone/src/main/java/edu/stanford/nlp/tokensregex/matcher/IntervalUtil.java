package edu.stanford.nlp.tokensregex.matcher;

import edu.stanford.nlp.util.ErasureUtils;
import edu.stanford.nlp.util.Function;
import edu.stanford.nlp.util.HasInterval;
import edu.stanford.nlp.util.Interval;

import java.util.*;

class IntervalUtil {
  @SuppressWarnings("unchecked")
  public static <T extends HasInterval<Integer>> Function<T, Double> lengthScorer() {
    return ErasureUtils.uncheckedCast(LENGTH_SCORER);
  }

  public static final Function<HasInterval<Integer>, Double> LENGTH_SCORER = new Function<HasInterval<Integer>,Double>() {
    @Override
    public Double apply(HasInterval<Integer> in) {
      Interval<Integer> interval = in.getInterval();
      return (double) (interval.getEnd() - interval.getBegin());
    }
  };
  
  private static class PartialScoredList<T,E> {
    T object;
    E lastMatchKey;
    int size;
    double score;
  }

  public static <T, E extends Comparable<E>> List<T> getNonOverlappingMaxScore(
      List<? extends T> items, Function<? super T,Interval<E>> toIntervalFunc, Function<? super T, Double> scoreFunc)
  {
    if (items.size() > 1) {
      Map<E,PartialScoredList<T,E>> bestNonOverlapping = new TreeMap<E,PartialScoredList<T,E>>();
      for (T item:items) {
        Interval<E> itemInterval = toIntervalFunc.apply(item);
        E mBegin = itemInterval.getBegin();
        E mEnd = itemInterval.getEnd();
        PartialScoredList<T,E> bestk = bestNonOverlapping.get(mEnd);
        double itemScore = scoreFunc.apply(item);
        if (bestk == null) {
          bestk = new PartialScoredList<T,E>();
          bestk.size = 1;
          bestk.score = itemScore;
          bestk.object = item;
          bestNonOverlapping.put(mEnd, bestk);
        }
        // Assumes map is ordered
        for (E j:bestNonOverlapping.keySet()) {
          if (j.compareTo(mBegin) > 0) break;
          // Consider adding this match into the bestNonOverlapping strand at j
          PartialScoredList<T,E> bestj = bestNonOverlapping.get(j);
          double withMatchScore = bestj.score + itemScore;
          boolean better = false;
          if (withMatchScore > bestk.score) {
            better = true;
          } else if (withMatchScore == bestk.score) {
            if (bestj.size + 1 < bestk.size) {
              better = true;
            }
          }
          if (better) {
            bestk.size = bestj.size + 1;
            bestk.score = withMatchScore;
            bestk.object = item;
            bestk.lastMatchKey = j;
          }
        }
      }

      PartialScoredList<T,E> best = null;
      for (PartialScoredList<T,E> v: bestNonOverlapping.values()) {
        if (best == null || v.score > best.score) {
          best = v;
        }
      }
      List<T> nonOverlapping = new ArrayList<T>(best.size);
      PartialScoredList<T,E> prev = best;
      while (prev != null) {
        if (prev.object != null) {
          nonOverlapping.add(prev.object);
        }
        if (prev.lastMatchKey != null) {
          prev = bestNonOverlapping.get(prev.lastMatchKey);
        } else {
          prev = null;
        }
      }
      Collections.reverse(nonOverlapping);
      return nonOverlapping;
    } else {
      List<T> nonOverlapping = new ArrayList<T>(items);
      return nonOverlapping;
    }
  }

  public static <T extends HasInterval<E>, E extends Comparable<E>> List<T> getNonOverlappingMaxScore(
      List<? extends T> items, Function<? super T, Double> scoreFunc)
  {
    Function<T,Interval<E>> toIntervalFunc = new Function<T, Interval<E>>() {
      @Override
      public Interval<E> apply(T in) {
        return in.getInterval();
      }
    };
    return getNonOverlappingMaxScore(items, toIntervalFunc, scoreFunc);
  }
}
