package oscar.cbls.lib.invariant.graph

import oscar.cbls.algo.graph.{ConditionalGraph, Connexity, Edge, Node}
import oscar.cbls.core.computation._

import scala.collection.immutable.SortedSet

class NbConnectedComponents(graph:ConditionalGraph,
                            openConditions:SetValue,
                            relevantNodes:SortedSet[Int])
  extends IntInvariant() with SetNotificationTarget {

  registerStaticDependency(openConditions)

  val components: Array[List[Node]] = Connexity.components(graph, _ => false)
  val componentsAllConditionsOpen: Array[List[Node]] = Connexity.components(graph, _ => false)

  this := componentsAllConditionsOpen.length //just to set something before restricting the domain
  this.restrictDomain(Domain(componentsAllConditionsOpen.length,components.length))

  private val nodeToComponents = Array.fill(graph.nbNodes)(-1)
  for(componentId <- components.indices){
    for(node <- components(componentId)){
      nodeToComponents(node.id) = componentId
    }
  }

  //Generating nodes of contracted graph
  //basically, there are as many nodes as there are components
  private val newNodes = Array.tabulate(components.length)(id => new Node(id,true))

  private val anythingRelevantInNewNode = components.map(nodeList => nodeList.exists(node => relevantNodes.contains(node.id)))

  private val newEdges = graph.conditionToConditionalEdges.map(conditionalEdge => {
    val nodeAInNewGraph = newNodes(nodeToComponents(conditionalEdge.nodeIDA))
    val nodeBInNewGraph = newNodes(nodeToComponents(conditionalEdge.nodeIDB))
    new Edge(
      conditionalEdge.conditionID.get,
      nodeAInNewGraph,
      nodeBInNewGraph,
      1,
      conditionalEdge.conditionID)
  })

  private val contractedGraph = new ConditionalGraph(newNodes,newEdges,graph.nbConditions)

  println(contractedGraph.statistics)

  override def notifySetChanges(v: ChangingSetValue, id: Int,
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]): Unit = {
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    val isConditionOpen = Array.fill(contractedGraph.nbConditions)(false)
    for(openCond <- openConditions.value){
      isConditionOpen(openCond) = true
    }

    this := Connexity.components(contractedGraph, cond => isConditionOpen(cond)).toList.flatMap(listOfNewNode => {
      val newList = listOfNewNode.filter(newNode => anythingRelevantInNewNode(newNode.id))
      if (newList.isEmpty) None else Some(newList)
    }).length
  }
}

