package oscar.cbls.business.routing.invariants.vehicleCapacity

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.invariants.global._
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}

import scala.annotation.tailrec

object MinMaxVehicleContent{
  def apply(routes: ChangingSeqValue,
            n: Int,
            v: Int,
            contentVariationAtNodeOrInitAtVehicleStart: Array[Long],
           ): MinMaxVehicleContent = {

    val minContentOnVehicleRoute: Array[CBLSIntVar] =
      Array.tabulate(v)(vehicle => CBLSIntVar(routes.model, name = s"minContentOnVehicle$vehicle"))
    val maxContentOnVehicleRoute: Array[CBLSIntVar] =
      Array.tabulate(v)(vehicle => CBLSIntVar(routes.model, name = s"maxContentOnVehicle$vehicle"))
    val finalContentOnVehicleRoute: Array[CBLSIntVar] =
      Array.tabulate(v)(vehicle => CBLSIntVar(routes.model, name = s"finalContentOnVehicle$vehicle"))
    new MinMaxVehicleContent(
      routes,
      n,
      v,
      contentVariationAtNodeOrInitAtVehicleStart,
      minContentOnVehicleRoute,
      maxContentOnVehicleRoute,
      finalContentOnVehicleRoute)
  }
}

class MinMaxVehicleContent(routes: ChangingSeqValue, override val n: Int, val v: Int,
                           contentVariationAtNodeOrInitAtVehicleStart: Array[Long],
                           val minContentOnVehicleRoute: Array[CBLSIntVar],
                           val maxContentOnVehicleRoute: Array[CBLSIntVar],
                           val finalContentOnVehicleRoute: Array[CBLSIntVar])
  extends LogReducedGlobalConstraint[TwoWaysVehicleContentFunction, (Long,Long,Long)](routes, n, v) {

  minContentOnVehicleRoute.foreach(violation => violation.setDefiningInvariant(this))
  maxContentOnVehicleRoute.foreach(violation => violation.setDefiningInvariant(this))
  finalContentOnVehicleRoute.foreach(violation => violation.setDefiningInvariant(this))

  val contentFunctionAtNode: Array[TwoWaysVehicleContentFunction] =
    Array.tabulate(n)(node => {
      val delta = contentVariationAtNodeOrInitAtVehicleStart(node)
      TwoWaysVehicleContentFunction(
        DefinedContentFunction(delta, delta, delta, node, node),
        DefinedContentFunction(delta, delta, delta, node, node))
    })

  // For the vehicle return value we consider that by default nothing is loaded/unloaded at the depot
  // (it's a fictive node)
  val contentFunctionForVehicleReturn: Array[TwoWaysVehicleContentFunction] =
  Array.tabulate(v)(vehicle => TwoWaysVehicleContentFunction(
    DefinedContentFunction(0,0,0,vehicle, vehicle),DefinedContentFunction(0,0,0,vehicle, vehicle)))

  /**
   * this method delivers the value of the node
   *
   * @return the type T associated with the node "node"
   */
  override def nodeValue(node: Int): TwoWaysVehicleContentFunction = contentFunctionAtNode(node)

  /**
   * this one is similar to the nodeValue except that it only is applied on vehicle,
   * to represent the return to the vehicle start at teh end of its route
   *
   * @param vehicle
   * @return
   */
  override def endNodeValue(vehicle: Int): TwoWaysVehicleContentFunction = contentFunctionForVehicleReturn(vehicle)

  /**
   * this method is for composing steps into bigger steps.
   *
   * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
   * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
   * @return the type T associated wit hthe first step followed by the second step
   */
  override def composeSteps(firstStep: TwoWaysVehicleContentFunction,
                            secondStep: TwoWaysVehicleContentFunction): TwoWaysVehicleContentFunction = {
    val flipped = composeVehicleContentFunctions(secondStep.flippedFunction, firstStep.flippedFunction)
    val nonFlipped = composeVehicleContentFunctions(firstStep.nonFlippedFunction, secondStep.nonFlippedFunction)
    TwoWaysVehicleContentFunction(nonFlipped, flipped)
  }

  private def composeVehicleContentFunctions(f1: VehicleContentFunction,
                                             f2: VehicleContentFunction): VehicleContentFunction ={
    if(f1.isEmpty || f2.isEmpty) return EmptyContentFunction
    val from = f1.from
    val to = f2.to
    val max = Math.max(f1.maxContentIfStartAt0, f1.contentAtEndIfStartAt0 + f2.maxContentIfStartAt0)
    val min = Math.min(f1.minContentIfStartAt0, f1.contentAtEndIfStartAt0 + f2.minContentIfStartAt0)
    val end = f1.contentAtEndIfStartAt0 + f2.contentAtEndIfStartAt0
    DefinedContentFunction(max, min, end, from, to)
  }

  /**
   * this method is called by the framework when the value of a vehicle must be computed.
   *
   * @param vehicle  the vehicle that we are focusing on
   * @param segments the segments that constitute the route.
   *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
   * @return the value associated with the vehicle. This value should only be computed based on the provided segments
   */
  override def computeVehicleValueComposed(vehicle: Int, segments: QList[LogReducedSegment[TwoWaysVehicleContentFunction]]): (Long,Long,Long) = {

    @tailrec
    def composeSubSegments(vehicleContentFunctions: QList[TwoWaysVehicleContentFunction], prev:(Long,Long,Long), flipped: Boolean): (Long,Long,Long) ={
      val twoWaysVehicleContentFunction = vehicleContentFunctions.head

      val prevContent = prev._3
      val newMin = prev._1 min (prevContent + twoWaysVehicleContentFunction.minIfStartAtZero(flipped))
      val newMax = prev._2 max (prevContent + twoWaysVehicleContentFunction.maxIfStartAtZero(flipped))
      val newOutContent = prev._3 + twoWaysVehicleContentFunction.contentAtEndIfStartAt0(flipped)

      if(vehicleContentFunctions.tail == null) (newMin,newMax,newOutContent)
      else composeSubSegments(vehicleContentFunctions.tail, (newMin,newMax,newOutContent), flipped)
    }

    @tailrec
    def minMaxFinishContent(logReducedSegments: QList[LogReducedSegment[TwoWaysVehicleContentFunction]],
                            prev: (Long,Long,Long)): (Long,Long,Long) ={
      if(logReducedSegments == null) prev
      else {
        val r = logReducedSegments.head match {
          case s@LogReducedPreComputedSubSequence(_, _, steps) =>
            composeSubSegments(steps, prev, flipped = false)

          case s@LogReducedFlippedPreComputedSubSequence(_, _, steps) =>
            composeSubSegments(steps.reverse, prev, flipped = true)

          case s@LogReducedNewNode(node, vehicleContentFunctionOfNode) =>

            val prevContent = prev._3
            val newMin = prev._1 min (prevContent + vehicleContentFunctionOfNode.minIfStartAtZero(false))
            val newMax = prev._2 max (prevContent + vehicleContentFunctionOfNode.maxIfStartAtZero(false))
            val newOutContent = prev._3 + vehicleContentFunctionOfNode.contentAtEndIfStartAt0(false)
            (newMin,newMax,newOutContent)

          case x =>
            throw new Error(s"Unhandled match with $x")
        }
        minMaxFinishContent(logReducedSegments.tail,r)
      }
    }
    minMaxFinishContent(segments,(0L,0L,0L))
  }

  /**
   * The framework calls this method to assign the value U corresponding to a specific checkpointLevel to the output variable of your invariant.
   * It has been dissociated from the method computeVehicleValue because the system should be able to restore a previously computed value without re-computing it.
   *
   * @param vehicle the vehicle number
   * @param value   The value to assign to the output variable
   */
  override def assignVehicleValue(vehicle: Int, value: (Long,Long,Long)): Unit = {
    minContentOnVehicleRoute(vehicle) := value._1
    maxContentOnVehicleRoute(vehicle) := value._2
    finalContentOnVehicleRoute(vehicle) := value._3
  }

  /**
   * This method is mainly defined for verification purpose.
   * But it's also used when we can't compute the vehicle value incrementally
   * (at the beginning of the search or when we assign the value of the route)
   * It computes the value of the vehicle from scratch.
   *
   * @param vehicle the vehicle on which the value is computed
   * @param routes  the sequence representing the route of all vehicle
   */
  override def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): (Long,Long,Long) = {

    var currentContent = contentVariationAtNodeOrInitAtVehicleStart(vehicle)
    var minContent = currentContent
    var maxContent = currentContent

    var explorer = routes.explorerAtAnyOccurrence(vehicle).get.next

    while(explorer.isDefined && explorer.get.value >= v){
      val currentNode = explorer.get
      currentContent += contentVariationAtNodeOrInitAtVehicleStart(currentNode.value)
      minContent = minContent min currentContent
      maxContent = maxContent max currentContent
      explorer = currentNode.next
    }
    (minContent,maxContent,currentContent)
  }
}
