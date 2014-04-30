package org.babysherlock.util

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import org.apache.commons.compress.compressors.bzip2.{BZip2CompressorOutputStream, BZip2CompressorInputStream}
import io.Source
import java.nio.channels.FileChannel
import java.nio.{ByteOrder, ByteBuffer}
import util.Random
import scala.util.matching.Regex

/**
 *  IO utility routines
 *  @author Angel Chang
 *  @author Manolis Savva
 */
object IOUtils {

  object Verbosity extends Enumeration {
    type Verbosity = Value
    val DEBUG, QUIET = Value
  }

  val endl = sys.props("line.separator")

  def getFilename(path: String, separators: Seq[Char] = "\\/"): String = {
    val slashIndex: Int = separators.map( s => path.lastIndexOf(s) ).max
    path.substring(slashIndex + 1)
  }

  def getParentDir(path: String, separators: Seq[Char] = "\\/"): String = {
    val slashIndex: Int = separators.map( s => path.lastIndexOf(s) ).max
    if (slashIndex >= 0) {
      path.substring(0, slashIndex)
    } else ""
  }

  val compressedExtensions = Set("gz", "gzip", "bz2", "bzip2")
  def stripExtension(filename: String, extensions: Iterable[String]): String = {
    for (x <- extensions) {
      if (filename.toLowerCase().endsWith("." + x)) {
        return filename.substring(filename.length - x.length - 1)
      }
    }
    filename
  }

  def inputStreamAutoDecompress(file: String, verbosity: Verbosity.Value = Verbosity.DEBUG): InputStream = {
    if (verbosity == Verbosity.DEBUG) println("Opening " + file)
    var is: InputStream = new BufferedInputStream(new FileInputStream(file))
    if (file.endsWith(".gz") || file.endsWith(".gzip")) {
      is = new GZIPInputStream(is)
    } else if (file.endsWith("bz2") || file.endsWith(".bzip2")) {
      is = new BZip2CompressorInputStream(is)
    }
    is
  }

  def inputStreamAutoDecompress(file: String, bytes: Array[Byte]): InputStream = {
    var is: InputStream = new ByteArrayInputStream(bytes)
    if (file.endsWith(".gz") || file.endsWith(".gzip")) {
      is = new GZIPInputStream(is)
    } else if (file.endsWith("bz2") || file.endsWith(".bzip2")) {
      is = new BZip2CompressorInputStream(is)
    }
    is
  }

  def isWebFile(fileName: String): Boolean = {
    fileName.startsWith("http:") || fileName.startsWith("https:")
  }

  def getLines(file: String, verbosity: Verbosity.Value = Verbosity.DEBUG): Iterator[String] = {
    io.Source.fromInputStream(inputStreamAutoDecompress(file, verbosity)).getLines()
  }

  def listFiles(file: String, r: Regex = null): Array[File] = {
    val f = new File(file)
    if (f.isDirectory) {
      if (r != null)
        f.listFiles().filter(x => r.pattern.matcher(x.getName).matches())
      else f.listFiles()
    } else {
      Array()
    }
  }

  def loadMap(file: String, separator: Char = ','): Map[String,String] = {
    IOUtils.getLines(file).map{ l =>
      val fs = l.split(separator)
      if (fs.size > 1) fs(0) -> fs(1) else null
    }.filter(_ != null).toMap
  }

  def iterablesToJSONObject[T](its: Iterable[Iterable[T]], names: List[String]) : String = {
    assert(its.size == names.size, "Number of Iterators and given names do not match.")
    val start = "{" + endl
    val sep = "," + endl
    val end = "}" + endl

    val children = its.zip(names).map{case(it, name) => iterableToJSONArray(it, name)}
    children.mkString(start, sep, end)
  }

  // Convert iterable it with optional name to a JSON string representing an array of its elements
  def iterableToJSONArray[T](it : Iterable[T] , name : String = "nanashiArei") : String = {
    val start = "\"" + name + "\": [" + endl
    val sep = "," + endl
    val end = endl + "]" + endl
    it.map(item => "\"" + item.toString + "\"").mkString(start, sep, end)
  }

  // Split a dataset of items into a set of splits indicated by (percentage,filename) and then save each
  // into filename
  def datasetSplit[T](dataset: List[T], splits: List[(Double,String)]) : List[(List[T],String)] = {
    val rand: Random = Random
    rand.setSeed(12345678)
    var D = rand.shuffle(dataset)
    val N = D.size
    val partitions = splits.map {case (percent,file) =>
      val Nitems = math.round(N * percent).toInt
      val partition = D.take(Nitems)
      D = D.drop(Nitems)
      (partition,file)
    }
    partitions.foreach{case (partition,file) => IOUtils.saveList(file, partition)}
    partitions
  }

  def createDirs(dirs: String*) {
    createDirs(dirs.toIterable)
  }

  def createDirs(dirs: Iterable[String]) {
    for (dir <- dirs) {
      val f = new File(dir)
      f.mkdirs()
    }
  }

  def printList[T](pw: PrintWriter, items: Iterable[T]) {
    for (item <- items) {
      pw.println(item)
    }
    pw.flush()
  }

  def save[T](file: String, item: String) {
    val pw = filePrintWriter(file)
    pw.print(item)
    pw.close()
  }

  def saveList[T](file: String, items: Iterable[T]) {
    val pw = filePrintWriter(file)
    printList(pw, items)
    pw.close()
  }

  def saveIterablesToCSV[T](file: String, it: Iterable[Iterable[T]], header: Iterable[String] = null,
                            annotationColumns: Iterable[(String,String)] = null) {
    var rows = it.map(row => row.mkString(",")).toList
    var headerStr = if (header != null) header.mkString(",") else ""
    if (annotationColumns != null) {
      headerStr = headerStr + "," + annotationColumns.map(_._1).mkString(",")
      rows = rows.map(row => row + "," + annotationColumns.map(_._2).mkString(","))
    }
    val out = if (headerStr != "") {
      headerStr +: rows
    } else rows
    IOUtils.saveList(file, out)
  }

  def filePrintWriter(filename: String, append: Boolean = false): PrintWriter = {
    val outputStream = fileOutputStream(filename, append)
    new PrintWriter(outputStream)
  }

  def fileOutputStream(filename: String, append: Boolean = false): OutputStream = {
    println("Opening file for output " + filename)
    val file: File = new File(filename)
    val parent: File = file.getParentFile
    if (parent != null) parent.mkdirs
    var outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(filename, append))
    if (filename.endsWith(".gz") || filename.endsWith(".gzip")) {
      outputStream = new GZIPOutputStream(outputStream)
    } else if (filename.endsWith("bz2")|| filename.endsWith(".bzip2")) {
      outputStream = new BZip2CompressorOutputStream(outputStream)
    }
    outputStream
  }

  def fileInputStream(filename: String): InputStream = {
    println("Opening file " + filename)
    var inputStream: InputStream = new BufferedInputStream(new FileInputStream(filename))
    if (filename.endsWith(".gz") || filename.endsWith(".gzip")) {
      inputStream = new GZIPInputStream(inputStream)
    } else if (filename.endsWith("bz2")|| filename.endsWith(".bzip2")) {
      inputStream = new BZip2CompressorInputStream(inputStream)
    }
    inputStream
  }

  def fileReader(filename: String) = new BufferedReader(new InputStreamReader(fileInputStream(filename)))

  def fileSource(filename: String, charset: String = "UTF-8"): Source = {
    Source.fromInputStream(fileInputStream(filename), charset)
  }

  def removeFile(filename: String) {
    val file: File = new File(filename)
    if (file.exists) {
      file.delete
    }
  }

  def moveFile(filename: String, destname: String) {
    val file: File = new File(filename)
    var dest: File = new File(destname)
    if (dest.isDirectory) {
      dest = new File(dest, file.getName)
    }
    file.renameTo(dest)
  }

  def isDir(filename: String): Boolean = {
    val file: File = new File(filename)
    file.canRead && file.isDirectory
  }

  def isReadableFileWithData(filename: String): Boolean = {
    val file: File = new File(filename)
    file.canRead && file.length > 0
  }

  def writeBytes(filename: String, bytes: Array[Byte]) {
    val file: File = new File(filename)
    val parent: File = file.getParentFile
    if (parent != null) parent.mkdirs
    val out = fileOutputStream(filename)
    out.write(bytes)
    out.close()
  }

  def writeBytes(filename: String, bytes: ByteBuffer) {
    writeBytes(filename, bytes.array())
  }

  def readBytesUnsigned3(filename: String, dim1: Int, dim2: Int, dim3: Int): Array[Array[Array[Short]]] = {
    IOUtils.readBytesUnsigned(filename, dim1*dim2*dim3).grouped(dim3).map( x => x.toArray ).
      grouped(dim2).map ( x => x.toArray).toArray
  }

  def readBytesUnsigned2(filename: String, dim1: Int, dim2: Int): Array[Array[Short]] = {
    IOUtils.readBytesUnsigned(filename, dim1*dim2).grouped(dim2).map( x => x.toArray ).toArray
  }

  def readBytesUnsigned(filename: String, expected: Int): Array[Short] = {
    readBytes(new File(filename), expected).map( b =>  (0xff & b).toShort )
  }

  def readBytes3(filename: String, dim1: Int, dim2: Int, dim3: Int): Array[Array[Array[Byte]]] = {
    IOUtils.readBytes(filename, dim1*dim2*dim3).grouped(dim3).map( x => x.toArray ).
      grouped(dim2).map ( x => x.toArray).toArray
  }

  def readBytes2(filename: String, dim1: Int, dim2: Int): Array[Array[Byte]] = {
    IOUtils.readBytes(filename, dim1*dim2).grouped(dim2).map( x => x.toArray ).toArray
  }

  def readBytes(filename: String, expected: Int): Array[Byte] = {
    readBytes(new File(filename), expected)
  }

  def readBytes(file: File, expected: Int): Array[Byte] = {
    if (expected > 0 && file.length != expected) {
      throw new IOException("Unexpected number of bytes in: " + file.getAbsolutePath +
        ", expected " + expected + ", got " + file.length)
    }
    val raf: RandomAccessFile = new RandomAccessFile(file.getAbsoluteFile, "r")
    val fc: FileChannel = raf.getChannel
    val input: ByteBuffer = fc.map(FileChannel.MapMode.READ_ONLY, 0, file.length)
    val array = Array.ofDim[Byte](file.length.toInt)
    input.get(array)
    raf.close()
    array
  }

  def readFloats(filename: String, expected: Int, bo: ByteOrder = ByteOrder.BIG_ENDIAN): Array[Float] = {
    readFloats(new File(filename), expected, bo)
  }

  def readFloats(file: File, expected: Int, bo: ByteOrder): Array[Float] = {
    println("Reading " + file.toString)
    val expectedFileSize = expected*4
    if (expected > 0 && file.length != expectedFileSize) {
      throw new IOException("Unexpected number of bytes in: " + file.getAbsolutePath +
        ", expected " + expectedFileSize + ", got " + file.length)
    }
    val raf: RandomAccessFile = new RandomAccessFile(file.getAbsoluteFile, "r")
    val fc: FileChannel = raf.getChannel
    val input: ByteBuffer = fc.map(FileChannel.MapMode.READ_ONLY, 0, file.length)
    input.order(bo)
    val n = file.length.toInt/4
    val array = Array.ofDim[Float](n)
    input.asFloatBuffer().get(array)
    raf.close()
    array
  }

  def readDoubles(filename: String, expected: Int, bo: ByteOrder = ByteOrder.BIG_ENDIAN): Array[Double] = {
    readDoubles(new File(filename), expected, bo)
  }

  def readDoubles(file: File, expected: Int, bo: ByteOrder): Array[Double] = {
    println("Reading " + file.toString)
    val expectedFileSize = expected*8
    if (expected > 0 && file.length != expectedFileSize) {
      throw new IOException("Unexpected number of bytes in: " + file.getAbsolutePath +
        ", expected " + expectedFileSize + ", got " + file.length)
    }
    val raf: RandomAccessFile = new RandomAccessFile(file.getAbsoluteFile, "r")
    val fc: FileChannel = raf.getChannel
    val input: ByteBuffer = fc.map(FileChannel.MapMode.READ_ONLY, 0, file.length)
    input.order(bo)
    val n = file.length.toInt/8
    val array = Array.ofDim[Double](n)
    input.asDoubleBuffer().get(array)
    raf.close()
    array
  }

  def getMapped(filename: String, expected: Int,  bo: ByteOrder = ByteOrder.BIG_ENDIAN): (RandomAccessFile, ByteBuffer) = {
    getMapped(new File(filename), expected, bo)
  }

  def getMapped(file: File, expected: Int,  bo: ByteOrder): (RandomAccessFile, ByteBuffer) = {
    val expectedFileSize = expected
    if (expected > 0 && file.length != expectedFileSize) {
      throw new IOException("Unexpected number of bytes in: " + file.getAbsolutePath +
        ", expected " + expectedFileSize + ", got " + file.length)
    }
    val raf: RandomAccessFile = new RandomAccessFile(file.getAbsoluteFile, "r")
    val fc: FileChannel = raf.getChannel
    val input: ByteBuffer = fc.map(FileChannel.MapMode.READ_ONLY, 0, file.length)
    input.order(bo)
    (raf,input)
  }

  def hexString2ByteArray(hex: String): Array[Byte] = {
    val ints = hex.sliding(2,2).toArray.map(Integer.parseInt(_, 16))
    ints.map(_.toByte)
  }
  def byteArray2HexString(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString
  def hexString2Float(hex: String): Float = {
    import java.lang.{Long,Float}
    Float.intBitsToFloat(Long.parseLong(hex, 16).toInt)
  }
  def hexString2FloatArray(hex: String): Array[Float] = {
    val floatBuf = ByteBuffer.wrap(hexString2ByteArray(hex)).asFloatBuffer()
    var arr = Array[Float]()
    while (floatBuf.hasRemaining) arr = arr :+ floatBuf.get()
    arr
  }
}