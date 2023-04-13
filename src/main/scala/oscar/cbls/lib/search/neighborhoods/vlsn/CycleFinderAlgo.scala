/**
  * *****************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  * ****************************************************************************
  */
package oscar.cbls.lib.search.neighborhoods.vlsn

object MatrixTools{
  def printBoolMatrix(m:Array[Array[Boolean]]): Unit ={
    for(l <- m){
      println(l.mkString("\t"))
    }
  }
}

object CycleFinderAlgoType extends Enumeration{
  type CycleFinderAlgoType = Value
  val Mouthuy, DFS, DFSPruned, MouthuyAndThenDFS = Value
}

import oscar.cbls.lib.search.neighborhoods.vlsn.CycleFinderAlgoType._

abstract class CycleFinderAlgo{
  def findCycle(liveNodes:Array[Boolean]):Option[List[Edge]]
}

object CycleFinderAlgo {

  def apply(graph: VLSNGraph, algo: CycleFinderAlgoType): CycleFinderAlgo = {
    algo match {
      case Mouthuy =>
        new CycleFinderAlgoMouthuy(graph)
      case DFS =>
        new CycleFinderAlgoDFS(graph, pruneOnReachability = false)
      case DFSPruned =>
        new CycleFinderAlgoDFS(graph, pruneOnReachability = true)
      case MouthuyAndThenDFS =>
        new CycleFinderAlgoMouthuyAndThenDFS(graph:VLSNGraph)
    }
  }
}

class CycleFinderAlgoMouthuyAndThenDFS(graph:VLSNGraph) extends CycleFinderAlgo{
  override def findCycle(liveNodes:Array[Boolean]): Option[List[Edge]] = {
    new CycleFinderAlgoMouthuy(graph).findCycle(liveNodes:Array[Boolean]) match{
      case None =>
        println("VLSN: Mouthy did not find; reverting to DFS")
        val moreCycle = new CycleFinderAlgoDFS(graph, pruneOnReachability = true).findCycle(liveNodes:Array[Boolean])
        if(moreCycle.nonEmpty) println("VLSN: DFS did improve") else println("VLSN: DFS did not improve")
        moreCycle
      case a => a
    }
  }
}
