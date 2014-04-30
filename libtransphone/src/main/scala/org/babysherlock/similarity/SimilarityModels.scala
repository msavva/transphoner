package org.babysherlock.similarity

import org.babysherlock.Constants
import org.goobs.sim.{DistSim, Ontology}
import java.io.File

/**
 * Default implementations for models needed by similarity calculations.
 * To use similarity metrics from a subclass of the Similarity trait, import these models (i.e., in the header) with
 *
 * import SimilarityModels._
 *   or
 * import SimlarityModels.{wordnet, distim}
 *
 * These implementations will be passed implicitly to the similarity/distance functions.
 * Note that the models are not loaded unless needed by the function, and are thereafter cached.
 * The environment variable BABYSHERLOCK_DATA must be set.
 *
 * Also note that these models don't have to be imported from here -- so long as there is an "implicit val x:Ontology"
 * and "implicit val x:DistSim" within scope when calling the similarity/distance functions the function call will succeed.
 */
object SimilarityModels {
  implicit val wordnet:()=>Ontology = {
    lazy val impl = Ontology.load(Constants.EXT_DATA_DIR + File.separator + "nlp" + File.separator + "models" + File.separator + "ontology.ser.gz")
    () => impl
  }
  implicit val distsim:()=>DistSim = {
    lazy val impl = DistSim.load(Constants.EXT_DATA_DIR + File.separator + "nlp" + File.separator + "models" + File.separator + "distsim.ser.gz")
    () => impl
  }
}

