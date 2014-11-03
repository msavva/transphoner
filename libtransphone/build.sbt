name := "libtransphoner"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models",
  "org.apache.commons" % "commons-compress" % "1.4.1",
  "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.3.1",
  "com.ibm.icu" % "icu4j" % "51.1",
  "org.apache.solr" % "solr-solrj" % "4.2.0",
  "net.sf.extjwnl" % "extjwnl" % "1.6.8",  
  "net.sf.opencsv" % "opencsv" % "2.3",
  "net.databinder.dispatch" % "dispatch-core_2.10" % "0.11.0",  
  "com.googlecode.json-simple" % "json-simple" % "1.1.1",
  "com.cedarsoftware" % "json-io" % "2.5.2",
  "org.scalanlp" % "breeze-math_2.10" % "0.2.3",
  "org.yaml" % "snakeyaml" % "1.12",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "edu.berkeley.nlp" % "berkeleylm" % "1.1.2",
  "org.goobs" % "sim" % "1.0" from "https://github.com/gangeli/sim/releases/download/v1.0/sim.jar",
  "edu.smu.tspell.wordnet" % "jaws" % "1.3" from "http://lyle.smu.edu/~tspell/jaws/jaws-bin.jar",
  "edu.princeton.wordnet" % "wordnet" % "3.1" artifacts(Artifact("wordnet","bundle","tar.gz",None, Nil, Some(new URL("http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz")))),
  "org.lexno" % "uwnapi" % "1.02" artifacts(Artifact("uwnapi","bundle","zip",None, Nil, Some(new URL("http://resources.mpi-inf.mpg.de/yago-naga/uwn/uwnapi.zip")))),
  "org.lexno" % "uwnapi-wordnet" % "1.02" artifacts(Artifact("uwnapi-wordnet","bundle","zip",None, Nil, Some(new URL("http://resources.mpi-inf.mpg.de/yago-naga/uwn/wordnet.zip")))),
  "org.lexno" % "uwnapi-core" % "1.02" artifacts(Artifact("uwnapi-core","bundle","zip",None, Nil, Some(new URL("http://resources.mpi-inf.mpg.de/yago-naga/uwn/uwn.zip"))))
  )

val basePath = taskKey[File]("external data path")

basePath := baseDirectory.value / "../ext-data/wordnet"

val extractData = taskKey[Unit]("Extract external data packages")

extractData := {
  val dest = basePath.value
  def untar(file: File): String = {
    println(s"Extracting ${file.getName} to ${dest.getName}")
    Unpack.gunzipTar(file, dest)
  }
  def unzip(file: File, path: File): String = {
    println(s"Extracting ${file.getName} to ${path.getName}")
    sbt.IO.unzip(file, path)
    file.getName
  }
  if (! dest.exists) {
    sbt.IO.createDirectory(dest)
  }
  Build.data((dependencyClasspath in Compile).value).map( f =>
    f.getName match {
      case name if name.startsWith("wordnet") => untar(f)
      case name if (name.startsWith("uwnapi-core") || name.startsWith("uwnapi-wordnet"))=> unzip(f, dest / "uwnapi" / "plugins")
      case name if (name.startsWith("uwnapi") && name.endsWith(".zip")) => unzip(f, dest / "uwnapi")
      case name => None //do nothing
    }
  )
  sbt.IO.move(dest / "dict", dest / "wn31dict")
  sbt.IO.move(dest / "uwnapi" / "uwnapi.jar", dest / ".." / ".." / "lib" / "uwnapi.jar")
  println("Done")
}

unmanagedBase := baseDirectory.value / "../lib"
