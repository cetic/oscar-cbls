package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP

import scala.collection.immutable.{HashSet, List}
import scala.collection.mutable
import oscar.cbls._

/**
  * Created by fg on 12L/09L/1L7.
  */
class Chains(vrp: VRP, chains: List[List[Long]]){

  val (chainOfNode, nextNodeInChain, prevNodeInChain, nextNodesInChain, prevNodesInChain) = {

    val chainOfNode: Array[List[Long]] = Array.fill(vrp.n)(List.empty)
    val nextNodeInChain: Array[Option[Long]] = Array.fill(vrp.n)(None)
    val prevNodeInChain: Array[Option[Long]] = Array.fill(vrp.n)(None)
    val nextNodesInChain: Array[List[Long]] = Array.fill(vrp.n)(List.empty)
    val prevNodesInChain: Array[List[Long]] = Array.fill(vrp.n)(List.empty)

    def proceedChain(toProceed: List[Long], previousNodes:List[Long],chain:List[Long]): Unit ={
      toProceed match {
        case Nil =>
        case head :: tail =>
          chainOfNode(head) = chain
          if(previousNodes.nonEmpty)nextNodeInChain(previousNodes.last) = Some(head)
          prevNodeInChain(head) = previousNodes.lastOption
          nextNodesInChain(head) = tail
          prevNodesInChain(head) = previousNodes

          proceedChain(tail,previousNodes :+ head,chain)
      }
    }

    for(chain <- chains) proceedChain(chain,List.empty,chain)
    for(node <- vrp.nodes if chainOfNode(node).isEmpty) proceedChain(List(node), List.empty, List(node))
    (chainOfNode,nextNodeInChain,prevNodeInChain,nextNodesInChain,prevNodesInChain)
  }

  //TODO: make this a set.
  val heads = chains.map(_.head)

  def firstNodeInChainOfNode(node: Long): Long = chainOfNode(node).head
  def lastNodeInChainOfNode(node: Long): Long = chainOfNode(node).last

  def isHead(node: Long): Boolean = !vrp.isADepot(node) && chainOfNode(node).head == node
  def isLast(node: Long): Boolean = chainOfNode(node).last == node
}
