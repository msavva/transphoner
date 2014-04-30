package org.babysherlock

import java.io.File

/**
 * Global parameters/constants for the TransPhoner project
 * @author Angel Chang
 */
trait Constants {
  protected def prop(namesToTry:Seq[String], defaultValue:String):String = namesToTry.foldLeft(None:Option[String]) { case (valueSoFar:Option[String], name:String) =>
    valueSoFar.orElse( Option(System.getProperty(name)) ).orElse( Option(System.getenv(name)) )
  }.getOrElse(defaultValue)
  protected def prop(name:String, defaultValue:String):String = prop(List(name), defaultValue)
  protected def ensureDir(str:String):String = if (str.endsWith(File.separator)) str else str + File.separator

  // Paths to our data...
  val WORKING_DIR = new java.io.File(".").getCanonicalPath
  val TRANSPHONER_DIR = ensureDir(prop("TRANSPHONER_DIR", WORKING_DIR))  // NOTE: Correct to be directory of code checkout if error
  val TRANSPHONER_DATA_DIR = TRANSPHONER_DIR + "data" + File.separator
  val EXT_DATA_DIR = ensureDir(prop("DATA_DIR", TRANSPHONER_DIR + "ext-data" + File.separator))
  val WORK_DIR = ensureDir(prop("WORK_DIR", TRANSPHONER_DIR + "work" + File.separator))

  // WordNet configuration file
  val SOLR_HOST = "http://localhost:8498"
  val WORDNET_CONF_FILE = TRANSPHONER_DIR + "conf" + File.separator + "extjwnl_file_properties.xml"
  val WORDNET_DICT30TO31MAP_FILE = EXT_DATA_DIR + "wordnet" + File.separator + "wn30to31.map.tsv"
  val WORDNET_DICT30_FILE = EXT_DATA_DIR + "wordnet" + File.separator + "wn30dict"
  val WORDNET_DICT31_FILE = EXT_DATA_DIR + "wordnet" + File.separator + "wn31dict"
  val WORDNET_DICT_FILE = WORDNET_DICT31_FILE
  val WORDNET_SOLR_URL = SOLR_HOST + "/solr/wordnet"

  // ConceptNet solr url
  val CONCEPTNET_SOLR_URL = "http://localhost:8599/solr/conceptnet5"
  // ConceptNet webapi url
  val CONCEPTNET_API_URL = "http://conceptnet5.media.mit.edu/data/5.2/"

  val PHONERAILS_DIR = TRANSPHONER_DIR + "phonerails" + File.separator
}

object Constants extends Constants
