package oscar.cbls.business.routing.invariants

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.computation.CBLSIntVar

object AdjacencyConstraint {
  /**
    * 
    * Build a adjacency contraints. The adjacency constraints is respected if the node of an adjacency pair are adjacent
    * The constraint returns the number of time the adjacency constraint is violated
    * 
    * @param vrp The vehicle routing problem
    * @param perVehicle a boolean flag that tells if the value shall be coputed per vehicle
    * @param respectOrder a boolean flag that tells is the adjacency shall respect the order
    * @param adjacencyList The list of pair of node that shall be adjacent
    * @return An array of CBLSIntVar that maintains the violation value. The array contains v value if the flag perVehicle is true and one value otherwise
    * 
    * **/

  def apply(vrp : VRP,
    perVehicle : Boolean,
    respectOrder : Boolean,
    adjacencyList : List[(Int,Int)]) : Array[CBLSIntVar] = {

    val distanceMatrix4Adjacency : Array[Array[Long]] = Array.fill(vrp.n)(Array.fill(vrp.n)(0))


      // Array.tabulate(vrp.n)(i => {
      //   Array.tabulate(vrp.n)(j => {
      //     if (adjacencyList.contains((i,j)) || (!respectOrder && adjacencyList.contains((j,i)))) {
      //       -2
      //     } else {
      //       if (i == j && (adjacencyList.map(_._1).contains(i) || adjacencyList.map(_._2).contains(i) || adjacencyList.map(_._1).contains(j) || adjacencyList.map(_._2).contains(j)))
      //         1
      //       else
      //         0
      //     }
      //   })
      // })

    adjacencyList.foreach({case (i,j) => {
      distanceMatrix4Adjacency(i)(i) = 1
      distanceMatrix4Adjacency(j)(j) = 1
      distanceMatrix4Adjacency(i)(j) = -2
      if (!respectOrder) distanceMatrix4Adjacency(j)(i) = -2
    }})

    RouteLength(vrp.routes,
      vrp.n,
      vrp.v,
      perVehicle,
      distanceMatrix4Adjacency,
      !respectOrder)

  }

}
