/*******************************************************************************
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
 ******************************************************************************/



package oscar.cp.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.algo.{DisjointSets, Inconsistency, RangeMinQuery}
import oscar.cp.{CPIntVar, CPIntVarOps, CPSetVar, Constraint}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar

import scala.math.Ordering.Double.TotalOrdering

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class HeldKarp(edges: CPSetVar, edgeData: Array[(Int,Int,Int)], cost: CPIntVar) extends Constraint(edges.store) {

  override def associatedVars(): Iterable[CPVar] = Array(edges)

  private[this] val epsilon = 10e-6
  private[this] val n = (edgeData.map(_._1).max max (edgeData.map(_._2).max)) +1
  private[this] val component = new DisjointSets[CCTreeNode](0,n-1)
  private[this] val cctree = new CCTree(n-1)
  private[this] val distMatrix = Array.fill(n,n)(new ReversibleInt(s,Int.MaxValue)) 
  
  private[this] val edgeIndex = Array.fill(n,n)(-1)
  private[this] val y = Array.fill(n)(0.0)
  
  for (((i,j,w),idx) <- edgeData.zipWithIndex) {
    edgeIndex(i)(j) = idx
    edgeIndex(j)(i) = idx
    distMatrix(i min j)(i max j) := w
  }
  
  @inline private def edgeWeight(i: Int, j: Int): Int = {
    distMatrix(i min j)(i max j).value
  }
  

  @inline private def removeEdge(i: Int,j: Int): Unit = {
    distMatrix(i min j)(i max j) := Int.MaxValue
    edges.excludes(edgeIndex(i)(j))
  }
  
  @inline private def forceEdge(i: Int,j: Int): Unit = {
    edges.requires(edgeIndex(i)(j))
  }  
  
  @inline private def isEdgePossible(i: Int,j: Int): Boolean = {
    edges.isPossible(edgeIndex(i)(j))
  }
  
  @inline private def isEdgeRequired(i: Int,j: Int): Boolean = {
    edges.isRequired(edgeIndex(i)(j))
  }  

  override def setup(l: CPPropagStrength): Unit = {
    for (((i,j,w),idx) <- edgeData.zipWithIndex) {
      if (!edges.isPossible(idx)) removeEdge(i,j)
      if (edges.isRequired(idx)) forceEdge(i,j)
    }
    edges.callPropagateWhenDomainChanges(this)
    propagateNumSteps(100)
  }
  
  override def propagate(): Unit = {
    propagateNumSteps(5)
  }
  
  @inline private def propagateNumSteps(nSteps: Int): Unit = {
    var iter = 0
    var improvement = true
    var lb = 0
    var stepSize = 0.1
    var alpha = 2.0
    var beta = 0.5
    val excluded = n-1
    val nMetaIter = 2
    
    var metaIter = 0
    while (metaIter < nMetaIter) {
      iter = 0
      while (iter < nSteps) {
        //println("iter---")
        iter += 1
        improvement = false
        
        component.reset()
        cctree.reset()
        component.resetAndSetData(i => cctree.nodes(i))
        val edgeUsed = Array.fill(edgeData.length)(false)
          
        val incident = Array.fill(n)(0)

        def edgeWeight(idx: Int): Double = {
          val (i, j, w) = edgeData(idx)
          (w - y(i) - y(j))
        }
        // first add the required edges to the tree
        val required = edges.requiredSet()
        var weight = 0.0
        var nAdjacentToExcluded = 0
        for (idx <- required) {
          val (i, j, w) = edgeData(idx)
          incident(i) += 1
          incident(j) += 1
          if (i != excluded && j != excluded) {
            val t1 = component.find(i).data.get
            val t2 = component.find(j).data.get
            val t = cctree.merge(t1,t2,idx)
            component.union(i,j,t)
          } else {
            nAdjacentToExcluded += 1
          }
          edgeUsed(idx) = true
          if (incident(i) > 2 || incident(j) > 2) {
            throw Inconsistency
          }
          weight += edgeWeight(idx)
        }
        // check if out degree is not more than 2
        if (nAdjacentToExcluded > 2) {
          throw Inconsistency
        }
        var heaviestWeightAdjacentToExcluded = Double.MaxValue 
        // then complete the minimum spanning tree with Kruskal
        val possibleValues = edges.possibleNotRequiredValues.toArray
        val possible = possibleValues.sortBy(i => edgeWeight(i))
        for (idx <- possible) {
          val (i, j, w) = edgeData(idx)
          if (i != excluded && j != excluded) {
            if (component.find(i) != component.find(j)) {
              incident(i) += 1
              incident(j) += 1
              val t1 = component.find(i).data.get
              val t2 = component.find(j).data.get
              val t = cctree.merge(t1, t2, idx)
              component.union(i, j, t)   
              edgeUsed(idx) = true
              weight += edgeWeight(idx)
              //println("add edge "+i+"->"+j+" w:"+w+ "ecluded:"+excluded)
            }
          } else {
            if (nAdjacentToExcluded < 2) {
              incident(i) += 1
              incident(j) += 1
              weight += edgeWeight(idx)
              heaviestWeightAdjacentToExcluded = edgeWeight(idx)
              nAdjacentToExcluded += 1
              edgeUsed(idx) = true
              //println("add 1-tree edge"+i+"->"+j+" w:"+w)

            }
          }
        }
        val oneTreeLBf = ((2 * y.map(_ + 0.0).sum + weight)-epsilon)
        //println("=>"+oneTreeLBf+" " +y.mkString(","))
        val oneTreeLB = oneTreeLBf.ceil.toInt
        if (lb < oneTreeLB) {
          improvement = true
          lb = oneTreeLB
          
          cost.updateMin(lb)
        }
        if (!cctree.singleRoot) {
          //println("failure , not single root")
          // the graph without "excluded" is not connected
          throw Inconsistency
        }

        // filtering of the edges
        if ((iter == nSteps) && (nMetaIter-1 == metaIter)) {
          val inorder = cctree.inorderCollect()
          val pos = Array.fill(inorder.length)(0)
          for (i <- 0 until inorder.length) {
            pos(inorder(i).index) = i
          }
          val heights = inorder.map(_.height)
          val rmq = new RangeMinQuery(heights)
          for (idx <- possible) {
            val (i, j, w) = edgeData(idx)
            if (!edgeUsed(idx)) {
              val reducedCost =
                if (i != excluded && j != excluded) {
                  // marginal cost of the edges that can enter into the spanning tree
                  val idxr = inorder(rmq(pos(i), pos(j))).value // this is the heaviest edge to be removed
                  edgeWeight(idx) - edgeWeight(idxr)
                } else {
                  // marginal cost of the edges adjacent to "excluded" node
                  edgeWeight(idx) - heaviestWeightAdjacentToExcluded
                }
              if ((oneTreeLBf + reducedCost).ceil.toInt > cost.max) {
                //println("failure h&k exclude edge")
                edges.excludes(idx)
              }
            }
          }
        }

        
        // update the weights
        val denom: Double = (for (i <- 0 until n) yield ((2 - incident(i)) * (2 - incident(i)))).sum
        var target = if (cost.max - oneTreeLB < 0) oneTreeLB+0.1 else cost.max
        if (denom == 0) stepSize = 0
        else stepSize = alpha * (target - oneTreeLB) / denom
        for (i <- 0 until n) {
          y(i) += (stepSize * (2 - incident(i)))
        }

      }
      // end of iters, can do edge filtering here


      alpha *= beta
      beta /= 2
      
      metaIter += 1
    }
    cost.updateMin(lb)
  }
 
}



/**
 * Connected Component Tree
 * @author Pierre Schaus pschaus@gmail.com
 */
class CCTreeNode(private[constraints] var parent: Int, private[constraints] var left: Int, private[constraints] var right: Int, val index: Int) {
  private[constraints] var v: Int = -1
  private[constraints] var h: Int = 0
  def height = h
  
  private def reset(): Unit = {
    parent = -1
    left = -1
    right = -1
  }
  def value = v
  def hasLeft = left >= 0
  def hasRight = right >= 0
  def hasParent = parent >= 0
}

/**
 * @param n the number of leaf nodes
 */
class CCTree(n: Int) {
  private var index = n
  private var rooted = false

  
  def reset(): Unit = {
    index = n
    rooted = false
  }
  

  val nodes = Array.tabulate(n + n - 1)(i => new CCTreeNode(-1, -1, -1, i))
  val nodesInorder = Array.tabulate(n + n - 1)(i => nodes(i))
  def singleRoot = rooted

  def merge(left: CCTreeNode, right: CCTreeNode, value: Int): CCTreeNode = {
    //println("left index:"+left.index+" right index:"+right.index +" index:"+index)
    assert(left.index < index && right.index < index)
    val parent = nodes(index)
    parent.left = left.index
    parent.right = right.index
    parent.v = value
    parent.parent = -1
    left.parent = index
    right.parent = index
    index += 1
    rooted = (index == nodes.length)
    if (rooted) computeHeights()
    parent
  }
  
  private def computeHeights(): Unit = {
    height(root)
    def height(n : CCTreeNode): Unit = {
      n.h = if (n.hasParent) nodes(n.parent).h + 1 else 0
      if (n.hasLeft) height(nodes(n.left))
      if (n.hasRight) height(nodes(n.right))
    }
  }

  def root: CCTreeNode = {
    assert(index == nodes.length)
    nodes(nodes.length-1)
  }

  /**
   * return the nodes sorted according to an inorder visit
   */
  def inorderCollect(): Array[CCTreeNode] = {
    var r = root
    var i = 0
    inorder(root)
    def inorder(n: CCTreeNode): Unit = {
      if (n.left != -1) inorder(nodes(n.left))
      nodesInorder(i) = n
      i += 1
      if (n.right != -1) inorder(nodes(n.right))
    }
    assert(i == index)
    nodesInorder
  }

}




/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class ChannelTSP(val succ: Array[CPIntVar],val distMatrix: Array[Array[Int]]) extends Constraint(succ(0).store) {

  override def associatedVars(): Iterable[CPVar] = succ

  val n = succ.size
  
  protected val edges = ((for (i <- 0 until n; j <- succ(i); if (i != j)) yield (n+i,j,distMatrix(i)(j))) ++ 
              (for (i <- 0 until n) yield (i+n,i,0))).toArray
  
  val edgeIndex = Array.fill(n,n)(0)
  for (((i,j,w),idx) <- edges.zipWithIndex) {
    edgeIndex(i-n)(j) = idx
  }
  
  protected val edgeVar = CPSetVar((0 until edges.size).toSet)(s)
  // todo: fix the cardinality of the set            
  
  
  override def setup(l: CPPropagStrength): Unit = {
	s.post(edgeVar.card === 2*n)
    
    for (i <- 0 until n) {
      succ(i).callValRemoveIdxWhenValueIsRemoved(this,i)
      succ(i).callValBindIdxWhenBind(this,i)
    }
    edgeVar.callValExcludedWhenExcludedValue(this)
    edgeVar.callValRequiredWhenRequiredValue(this)
    
    for ((i,j,w) <- edges) {
      if (((i-n) != j) && !succ(i-n).hasValue(j)) {
        edgeVar.excludes(edgeIndex(i-n)(j))
      }
    }
  }
  
  override def valRemoveIdx(x: CPIntVar, idx: Int, v: Int): Unit = {
    if (v != idx) {
      edgeVar.excludes(edgeIndex(idx)(v))
    }
  }
  
  override def valBindIdx(x: CPIntVar, idx: Int): Unit = {
    edgeVar.requires(edgeIndex(idx)(x.value))
  } 
  
  override def valExcluded(x: CPSetVar, v: Int): Unit = {
    val (i,j,w) = edges(v)
    succ(i-n).removeValue(j)
  }  

  override def valRequired(x: CPSetVar, v: Int): Unit = {
    val (i,j,w) = edges(v)
    if ((i-n) != j) {
      succ(i - n).assign(j)
    }
  }   

}
