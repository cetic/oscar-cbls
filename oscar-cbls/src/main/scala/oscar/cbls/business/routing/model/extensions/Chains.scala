package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP

import scala.annotation.tailrec

/**
  * Created by fg on 12/09/17.
  */
class Chains(vrp: VRP, chains: List[List[Int]]){

  val (chainOfNode, nextNodeInChain, prevNodeInChain, nextNodesInChain, prevNodesInChain) = {

    val chainOfNode: Array[List[Int]] = Array.fill(vrp.n)(List.empty)
    val nextNodeInChain: Array[Option[Int]] = Array.fill(vrp.n)(None)
    val prevNodeInChain: Array[Option[Int]] = Array.fill(vrp.n)(None)
    val nextNodesInChain: Array[List[Int]] = Array.fill(vrp.n)(List.empty)
    val prevNodesInChain: Array[List[Int]] = Array.fill(vrp.n)(List.empty)

    @tailrec
    def proceedChain(toProceed: List[Int], previousNodes:List[Int], chain:List[Int]): Unit ={
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

  val heads: List[Int] = chains.map(_.head)

  def firstNodeInChainOfNode(node: Int): Int = chainOfNode(node).head
  def lastNodeInChainOfNode(node: Int): Int = chainOfNode(node).last

  def isHead(node: Int): Boolean = !vrp.isADepot(node) && chainOfNode(node).head == node
  def isLast(node: Int): Boolean = chainOfNode(node).last == node
}
