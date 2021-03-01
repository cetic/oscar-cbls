package oscar.cp.searches.lns.selection

import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSElement

import scala.collection.mutable.ListBuffer

object Metrics {

  var worstTTI = 1.0

  def avgTime(element: ALNSElement): Double = if(element.execs != 0) element.time / element.execs else 0.0

  def avgImprovement(element: ALNSElement): Double = if(element.execs != 0) element.improvement / element.execs else 0.0

  def successRate(element: ALNSElement): Double = if (element.execs != 0) element.successfulRuns / element.execs else 0.0

  def timeToImprovement(element: ALNSElement): Double = {
    if(element.execs == 0) 0.0
    else if(successRate(element) == 0.0) worstTTI
    else{
      val tti = (avgTime(element) + 1.0) / successRate(element)
      if(tti > worstTTI) worstTTI = tti
      tti
    }
  }

  def lastImprovement(element: ALNSElement): Double = {
    val execStats = element.lastExecStats
    if(execStats.isDefined) execStats.get.improvement.toDouble
    else 0
  }

  def lastImprovementRatio(element: ALNSElement): Double = {
    val execStats = element.lastExecStats
    if(execStats.isDefined) execStats.get.improvement.toDouble / (execStats.get.time + 1)
    else 0.0
  }

  def averageImprovement(element: ALNSElement): Double = {
    avgImprovement(element)
  }

  def averageImprovementRatio(element: ALNSElement): Double = {
    element.improvement / (element.time / 1000000.0 + 1)
  }

  def efficiencyFor(element: ALNSElement, duration: Long): Double = {
    val stats = element.stats
    if(stats.isEmpty) 0.0
    else {
      var time = 0L
      var improvement = 0
      var i = stats.length - 1
      while (time < duration && i >= 0) {
        time += stats(i).time
        improvement += stats(i).improvement
        i -= 1
      }
      if(time > 0) improvement / (time / 1000000000.0) //Converting time to seconds
      else 0.0
    }
  }

  def efficiencyFor(element: ALNSElement, iterations: Int): Double = {
    val stats = element.stats
    if(stats.isEmpty) 0.0
    else {
      var time = 0L
      var improvement = 0
      var i = stats.length - 1
      while (i >= stats.length - iterations && i >= 0) {
        time += stats(i).time
        improvement += stats(i).improvement
        i -= 1
      }
      if(time > 0) improvement / (time / 1000000000.0) //Converting time to seconds
      else 0.0
    }
  }

  def efficiencySince(element: ALNSElement, t: Long): Double = {
    val stats = element.stats
    if(stats.isEmpty) 0.0
    else {
      var time = 0L
      var improvement = 0
      var i = stats.length - 1
      while(i >= 0 && stats(i).tStart > t) {
        time += stats(i).time
        improvement += stats(i).improvement
        i -= 1
      }
      if(time > 0) improvement / (time / 1000000000.0) //Converting time to seconds
      else 0.0
    }
  }


  def searchEfficiencyFor(sols: Seq[CPIntSol], t: Long, currentTime: Long): Double = {
    if(sols.isEmpty) 0.0
    else {
      val currentObj = sols.last.objective
      var tStart = currentTime
      var objStart = currentObj
      var i = sols.length - 1
      while(i >= 0 && tStart > currentTime - t) {
        tStart = sols(i).time
        objStart = sols(i).objective
        i -= 1
      }
      val time = Math.abs(currentTime - tStart)
      if(time > 0) Math.abs(objStart - currentObj).toDouble / (time / 1000000000.0)
      else 0.0
    }
  }

  def searchEfficiencySince(iterStates: Seq[(Long, Int)], t: Long, currentTime: Long, currentObj: Int): Double = {
    if(iterStates.isEmpty) 0.0
    else {
      var tStart = currentTime
      var objStart = currentObj
      var i = iterStates.length - 1
      while(i >= 0 && tStart > t) {
        tStart = iterStates(i)._1
        objStart = iterStates(i)._2
        i -= 1
      }
      val time = Math.abs(currentTime - tStart)
      if(time > 0) Math.abs(currentObj - objStart).toDouble / (time / 1000000000.0)
      else 0.0
    }
  }
}
