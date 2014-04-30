package org.babysherlock.util

import org.yaml.snakeyaml.Yaml
import scala.collection.JavaConversions._
import java.io.StringReader
import java.util.{TimeZone, Date}
import java.text.SimpleDateFormat

/**
 * SQLLite Db stored as yaml
 * @author Angel Chang
 */
object YamlDb {
  val yaml = new Yaml()

  def apply(filename: String): YamlDb = {
    val input = IOUtils.fileInputStream(filename)
    val data = yaml.loadAll(input).toSeq
    val tables = data.map( x => x.asInstanceOf[java.util.Map[String,Object]]
      .map( y => new YamlDb.Table(y._1, y._2.asInstanceOf[java.util.Map[String,Object]])) )
      .flatten
    new YamlDb( tables.map( t => t.name -> t ).toMap )
  }

  def readYaml(string: String): Object = {
    yaml.load(new StringReader(string))
  }

  case class Table(name: String, columns: IndexedSeq[String], records: IndexedSeq[IndexedSeq[Object]]) {
    lazy val columnIndex = columns.zipWithIndex.map( x => x._1 -> x._2 ).toMap
    lazy val idToRow = columnStrings("id").zipWithIndex.map( x => x._1 -> x._2 ).toMap

    def apply(i: Int): IndexedSeq[Object]= row(i)
    def apply(i: Int, j: Int): Object = row(i)(j)
    def apply(i: Int, fieldName: String): Object = row(i)(columnIndex(fieldName))
    def apply(id: String): IndexedSeq[Object]= row(idToRow(id))
    def apply(id: String, j: Int): Object = row(idToRow(id))(j)
    def apply(id: String, fieldName: String): Object = row(idToRow(id))(columnIndex(fieldName))

    def get(i: Int):Option[IndexedSeq[Object]] = if (i >= 0 && i < records.size) Option(row(i)) else None
    def get(i: Int, j: Int): Option[Object] = if (j >= 0 && j < columns.size) get(i).map( r => Option(r.get(j)) ) else None
    def get(i: Int, fieldName: String): Option[Object] =
      if (columnIndex.contains(fieldName)) get(i).map( r => Option(r(columnIndex(fieldName))) ) else None
    def getById(id: String):Option[IndexedSeq[Object]] = get(idToRow.getOrElse(id,-1))
    def getById(id: String, columnIndex: Int): Option[Object] = get(idToRow.getOrElse(id,-1), columnIndex)
    def getById(id: String, fieldName: String):Option[Object] = get(idToRow.getOrElse(id,-1), fieldName)
    def getOrElse(i: Int, fieldName: String, default: Object): Object = get(i,fieldName).getOrElse(default)
    def getOrElse(i: Int, j: Int, default: Object): Object = get(i,j).getOrElse(default)

    def row(i: Int) = records(i)
    def columnObjects(fieldName: String): IndexedSeq[Object] = columnObjects(columnIndex(fieldName))
    def columnObjects(i: Int): IndexedSeq[Object] = records.map( r => r(i) )
    def columnObjectsByName(fieldNames: Seq[String]) = columnObjectsByIndex(fieldNames.map(columnIndex(_)))
    def columnObjectsByIndex(is: Seq[Int]): IndexedSeq[IndexedSeq[Object]] = records.map( r => is.map(r(_)).toIndexedSeq)
    def columnStrings(fieldName: String): IndexedSeq[String] = columnStrings(columnIndex(fieldName))
    def columnStrings(i: Int): IndexedSeq[String] = columnObjects(i).map( x => if (x != null) x.toString else null )
    def columnStringsByNames(fieldNames: Seq[String]) = columnStringsByIndex(fieldNames.map(columnIndex(_)))
    def columnStringsByIndex(is: Seq[Int]): IndexedSeq[IndexedSeq[String]] =
      columnObjectsByIndex(is).map(s => s.map( x => if (x != null) x.toString else null ))

    def this(name: String, m: java.util.Map[String,Object]) =
      this(name,
        m("columns").asInstanceOf[java.util.List[String]].toIndexedSeq,
        m("records").asInstanceOf[java.util.List[Object]].toIndexedSeq
          .map( r => r.asInstanceOf[java.util.List[Object]].toIndexedSeq ) )
  }

  def asStringMap(m: Map[String,Object]): Map[String,String] =
    m.mapValues( x => if (x != null) x.toString else null).toMap
  def javaMapAsStringMap(m: Object): Map[String,String] =
    m.asInstanceOf[java.util.Map[String,Object]].mapValues( x => if (x != null) x.toString else null).toMap
  def javaListAsStringMapSeq(m: Object): IndexedSeq[Map[String,String]] =
    m.asInstanceOf[java.util.List[Object]].map( x => javaMapAsStringMap(x) ).toIndexedSeq

  object AggrType extends Enumeration {

    class Value(val aggrFunc: Seq[Double] => Double) extends super.Val {
    }
    val Ave = new Value(s => s.sum/s.size)
    val Sum = new Value(s => s.sum)
  }
}

class YamlDb(val tables: Map[String,YamlDb.Table]) {
}

class YamlMTurkDb(val yamlDb: YamlDb) {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"))

  def mturkTasks = yamlDb.tables("mt_tasks")

  def mturkAssignmentsIncomplete = {
    mturkAssignments.filter( x => x.data == null)
  }

  def mturkAssignmentsData = {
    mturkAssignments.filter( x => x.data != null)
  }

  // Includes incomplete assignments
  def mturkAssignments = {
    val assignments = yamlDb.tables("mt_assignments")
    val workers = yamlDb.tables("mt_workers")
    val tasks = yamlDb.tables("mt_tasks")
    val hits = yamlDb.tables("mt_hits")
    val workerData = assignments.columnStringsByNames(
      Seq("mt_worker_id", "mt_hit_id", "data", "created_at", "completed_at", "mtId"))
    val data = workerData
      .map( x => {
        val taskId = hits(x(1),"mt_task_id").asInstanceOf[Integer].toString
        val assignmentId = x(5)
        val taskName = tasks(taskId,"name").asInstanceOf[String]
        val workerId = workers(x(0),"mtId").asInstanceOf[String]
        val createdAtDate = parseDate(x(3))
       // println("createdAtDate is " + createdAtDate + " orig=" + x(3))
        if (x(2) != null) {
          val completedAtDate = parseDate(x(4))
         // println("completedAtDate is " + completedAtDate + " orig=" + x(4))
          YamlMTurkDb.MTurkAssignment(assignmentId, taskId, taskName, workerId, parseAssignmentsData(x(2)).toMap, createdAtDate, completedAtDate)
        } else {
          YamlMTurkDb.MTurkAssignment(assignmentId, taskId, taskName, workerId, null, createdAtDate, null)
        }
      }
    )
    data
  }

  def parseAssignmentsData(workerData: String) = {
    var s = YamlDb.readYaml(workerData)
    // Sometimes there is another layer of string wrapper around this
    if (s.isInstanceOf[java.lang.String]) s = YamlDb.readYaml(s.asInstanceOf[java.lang.String])
    s.asInstanceOf[java.util.Map[String,Object]]
  }

  def parseDate(dateStr: String): Date = {
    var date: Date = null
    if (dateStr != null) {
      // Find .SSSSSS
      val i = dateStr.indexOf('.')
      if (i >= 0) {
        date = dateFormat.parse(dateStr.substring(0, i))
        val millis = Integer.parseInt(dateStr.substring(i+1,i+4))
        date.setTime( date.getTime + millis )
      } else {
        date = dateFormat.parse(dateStr)
      }
    }
    date
  }


}

object YamlMTurkDb {
  case class MTurkAssignment( assignmentId: String,
                              taskId: String,
                              taskName: String,
                              workerId: String,
                              data: Map[String,Object],
                              createdAt: Date,
                              completedAt: Date
                             ) {
    def elapsedMs = completedAt.getTime - createdAt.getTime
    def elapsedSeconds = elapsedMs/1000.0
    def elapsedMinutes = elapsedMs/60000.0
  }
  case class MTurkTask (
                         taskId: String,
                         taskName: String,
                         completed: Seq[MTurkAssignment],
                         incomplete: Seq[MTurkAssignment]
                        )

  def getMTurkAssignments(filenames: Seq[String]): Seq[YamlMTurkDb.MTurkAssignment] = {
    // Go through files and aggregate turker results
    val turkerResults = (for (filename <- filenames) yield {
      val mturkDb = new YamlMTurkDb(YamlDb(filename))
      mturkDb.mturkAssignments
    }).flatten

    turkerResults
  }

  def getMTurkTasks(filenames: Seq[String]) = {
    val turkerAssignments = getMTurkAssignments(filenames)
    val turkerAssignmentsByTask = turkerAssignments.groupBy( t => t.taskId )
    turkerAssignmentsByTask.map(
      kv => {
        val (complete,incomplete) = kv._2.partition( x => x.data != null )
        val taskName = kv._2.head.taskName
        MTurkTask(kv._1, taskName, complete, incomplete)
      }
    )
  }
}
