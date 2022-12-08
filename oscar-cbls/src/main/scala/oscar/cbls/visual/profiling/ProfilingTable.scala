package oscar.cbls.visual.profiling

import oscar.cbls.core.search.NeighborhoodProfiler
import oscar.visual.VisualTable

import java.awt.Color

object ProfilingTable {
  def apply(profiledEasyNeighborhood: List[NeighborhoodProfiler]): ProfilingTable ={
    val headers = profiledEasyNeighborhood.head.collectThisProfileHeader
    val values: Array[Array[String]] = profiledEasyNeighborhood.map(_.collectThisProfileData).toArray.filter(_.head != "DoNothingNeighborhood")
    val goodValueIndicator = profiledEasyNeighborhood.head.goodValueIndicator()
    new ProfilingTable(headers, values, goodValueIndicator)
  }
}

class ProfilingTable(headers: Array[String], values: Array[Array[String]], goodValueIndicator: Array[Option[String]]) extends VisualTable(values.length,headers.length-1){
  val rangeRow = values.indices
  val rangeColumn = (0 until headers.length-1)

  val best3Values: Array[List[Int]] = goodValueIndicator.indices.map(vi => goodValueIndicator(vi) match {
    case None => List(-1, -1, -1)
    case Some("Max") => values.indices.sortBy(nv => -statToDouble(values(nv)(vi))).take(3).toList
    case Some("Min") => values.indices.sortBy(nv => statToDouble(values(nv)(vi))).take(3).toList
    case _ => List(-1, -1, -1)
  }).toArray

  val worst3Values: Array[List[Int]] = goodValueIndicator.indices.map(vi => goodValueIndicator(vi) match {
    case None => List(-1, -1, -1)
    case Some("Max") => values.indices.sortBy(nv => statToDouble(values(nv)(vi))).take(3).toList
    case Some("Min") => values.indices.sortBy(nv => -statToDouble(values(nv)(vi))).take(3).toList
    case _ => List(-1, -1, -1)
  }).toArray

  def setHeaders(): Unit =
    rangeColumn.foreach(c => this.setColumnName(headers(c+1),c))

  def setEntriesName(): Unit =
    rangeRow.foreach(r => this.setRowName(values(r)(0),r))

  def setValues(): Unit ={
    rangeRow.foreach(r => rangeColumn.foreach(c => this.setValueAt(values(r)(c+1),r,c)))
  }

  def colorTable(): Unit ={
    // drop(1) ==> drop neighborhood name value which is the entry name
    best3Values.drop(1).indices.foreach(column =>{
      best3Values(column+1).zipWithIndex.foreach(rowAndRank =>
        if(rowAndRank._1 >= 0)
          setColorAt(bestValueColor(rowAndRank._2),rowAndRank._1,column)
      )
    })
    worst3Values.drop(1).indices.foreach(column =>{
      worst3Values(column+1).zipWithIndex.foreach(rowAndRank =>
        if(rowAndRank._1 >= 0)
          setColorAt(worstValueColor(rowAndRank._2),rowAndRank._1,column)
      )
    })
  }

  private def bestValueColor(rank: Int): Color ={
    rank match {
      case 0 => new Color(0,220,0)
      case 1 => new Color(150,220,0)
      case 2 => new Color(200,220,0)
      case _ =>
        println("ERROR, should only have 3 level")
        new Color(0,0,0)
    }
  }

  private def worstValueColor(rank: Int): Color ={
    rank match {
      case 0 => new Color(220,0,0)
      case 1 => new Color(220,150,0)
      case 2 => new Color(220,200,0)
      case _ =>
        println("ERROR, should only have 3 level")
        new Color(0,0,0)
    }
  }

  private def statToDouble(stat: String): Double = {
    try {
      stat.toDouble
    } catch {
      case e: NumberFormatException => -1
    }
  }

  setHeaders()
  setEntriesName()
  setValues()
  colorTable()
}
