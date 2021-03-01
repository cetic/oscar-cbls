package oscar.cp.constraints.mdd


import java.util

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleContext
import oscar.cp.constraints.Automaton


/**
  * This class is the base of the static MDD framework.
  * All the functions present in the static mdd object are described here
  *
  * Created by rhenneton on 15/01/17.
  */
abstract class StaticMdd {
  /**
    * Apply the refinement algorithm on the decision diagram : "Split and prune" iterative process based on the
    * constraints that were applied to the MDD
    * @param maxWidth
    */
  def refine(maxWidth: Int): Unit

  /**
    * Prune all inconsistent edges of the MDD without doing any splitting ( = Refinement with maxWidth of size 0)
    */
  def prune(): Unit = this.refine(0)

  /**
    * Check if a tuple is a path inside the MDD
    * @param tuple
    * @return
    */
  def contains(tuple : Array[Int]) : Boolean

  /**
    * Add a tuple to the existing mdd
    * @param tuple
    */
  def addTuple(tuple: Array[Int]): Unit

  /**
    * Remove a tuple (if present) from the MDD
    * @param tuple
    */
  def removeTuple(tuple: Array[Int]): Unit

  /**
    * Compute the number of possible paths from root to end node.
    * As this could grow exponentially, we use java BigInteger
    */
  def numPaths() : java.math.BigInteger

  /**
    * Add a static constraint to the MDD constraints. Those will be used with the refine/prune function to
    * restrict the paths in the mdd to represent "as much as possible" only paths that meets the constraints
    * @param cstr
    */
  def addConstraint(cstr: StaticMddConstraint): Unit

  /**
    * During the refinement process, we split the nodes. In fact, we extract one in-edge from a node. This is done
    * greedily according to several heuristics. Those heuristics can be customized with this function. The higher
    * the priority, the bigger importance it will have on the extraction decision
    * @param priority
    * @param heuristic
    */
  def addComparator(priority : Int, heuristic : (StaticMddSplittedNode,StaticMddSplittedNode) => Int) : Unit

  /**
    * Bottom up reduction used in the creation of the table mdd and the regular constraint.
    */
  def reduce(): Unit

  /**
    * Returns the domains of each layer of the static mdd
    */
  def staticDomains(): Array[util.TreeSet[Int]]

  def getNumberOfEdges(): Int

  def getNumberOfNodes(): Int

  /**
    * Map the nodes of the static mdd to a new numbering inside the reversible MDD. If the number of nodes is n, the
    * new id's of the reversibleNodes will be 0,1,..., n-1. Return the node mapping (id in static mdd -> id in reversibleMdd)
    * @param nodes
    * @param reversibleContext
    * @return nodesMapping
    */
  def mapNodes(nodes: Array[ReversibleMddNode], reversibleContext: ReversibleContext): util.HashMap[Long, Int]

  /**
    * Map the static edges to a new numbering of the id's inside the reversible MDD. If the number of edges is n, the
    * new id's of the reversibleEdges will be 0,1,..., n-1. Retrn the edges mapping (id in the static mdd -> id in reversibleMdd)
    * @param edges
    * @param nodes
    * @param nodesMapping
    * @return edgesMapping
    */
  def mapEdgesAndLink(edges: Array[ReversibleMddEdge], nodes: Array[ReversibleMddNode], nodesMapping: util.HashMap[Long, Int]): util.HashMap[Long, Int]

  /**
    * @return : The arity of the considered mdd (number of layer of edges)
    */
  def getArity() : Int
}


object StaticMdd {

  /**
    * Builds an mdd based on a table constraint by using the method presented by Cheng and Yap in
    * MDD-based generalized arc consistencyalgorithm for positive and negative table constraints and some global constraints
    * @param table : Table constraint
    * @param arity : Arity of the table constraint (and depth of the MDD)
    * @return
    */
  def buildMddFromTableChenAndYap(table: Array[Array[Int]], arity: Int): StaticMddImpl = {
    val mdd = new StaticMddImpl(arity)
    /**
      * First step is to build the trie with the end node being the common "leaf"
      */
    for (tuple <- table) {
      if (tuple.length == arity) {
        var curNode = mdd.root
        var i = 0
        while (i < arity) {
          val value = tuple(i)
          if (curNode.containsOutValue(value)) curNode = curNode.getOutEdge(value).bottomNode
          else {
            if (i != arity - 1) {
              val newNode = new StaticMddNode(i + 1)
              mdd.layers(i + 1).add(newNode)
              val newEdge = new StaticMddEdge(curNode, newNode, value)
              curNode = newNode
            }
            else {
              val newEdge = new StaticMddEdge(curNode, mdd.end, value)
            }
          }
          i += 1
        }
      }
    }

    /**
      * Bottom up reduction of the trie in order to limit the number of edges in the mdd and therefore the propagation time
      */
    mdd.reduce()
    mdd
  }

  /**
    * Builds an mdd based on a table constraint by using the method presented by Perez and RÃ©gin in
    * Relations between MDDs and Tuples andDynamic Modifications of MDDs based constraintsâ€
    * @param table : Table constraint
    * @param arity : Arity of the table constraint (and depth of the mdd)
    * @return
    */
  def buildMddFromTableRegin(table: Array[Array[Int]], arity: Int): StaticMddImpl = {
    if (table.length == 0)
      throw Inconsistency
    val mdd = new StaticMddImpl(table(0).length)
    var mini = table(0)(0)
    var maxi = table(0)(0)
    var i = 0
    val al = new util.ArrayList[Array[Int]]()
    while (i < table.length) {
      mini = mini min table(i)(0)
      maxi = maxi max table(i)(0)
      al.add(table(i))
      i += 1
    }

    /**
      * Build the trie recursively
      */
    utilRecursiveBuilding(mdd, al, mdd.root, mini, maxi, 0)
    mdd.reduce()
    mdd
  }

  /**
    * Builds the trie recursively by performing a bucket (radix) sort at each node
    * The min and maxi are there to limit the complexity and only visit the buckets from min value to max value
    * @param mdd
    * @param bucket
    * @param curNode
    * @param mini
    * @param maxi
    * @param curDepth
    */
  private def utilRecursiveBuilding(mdd: StaticMddImpl, bucket: util.List[Array[Int]], curNode: StaticMddNode, mini: Int, maxi: Int, curDepth: Int): Unit = {
    if (curDepth == mdd.arity - 1) {
      var i = 0
      while (i < bucket.size()) {
        val newEdge = new StaticMddEdge(curNode, mdd.end, bucket.get(i)(curDepth))
        i += 1
      }
    }
    else {
      val buckets = Array.fill(maxi - mini + 1)(new util.ArrayList[Array[Int]]())
      val bucketsMin = Array.fill(maxi - mini + 1)(Int.MaxValue)
      val bucketsMax = Array.fill(maxi - mini + 1)(Int.MinValue)
      var newMini = bucket.get(0)(curDepth + 1)
      var newMaxi = bucket.get(0)(curDepth + 1)
      var i = 0
      while (i < bucket.size()) {
        val value = bucket.get(i)(curDepth) - mini
        buckets(value).add(bucket.get(i))
        bucketsMin(value) = bucketsMin(value) min bucket.get(i)(curDepth + 1)
        bucketsMax(value) = bucketsMax(value) max bucket.get(i)(curDepth + 1)
        i += 1
      }
      i = 0
      while (i < buckets.length) {
        if (buckets(i).size() > 0) {
          val newNode = new StaticMddNode(curDepth + 1)
          mdd.layers(curDepth + 1).add(newNode)
          val newEdge = new StaticMddEdge(curNode, newNode, mini + i)
          utilRecursiveBuilding(mdd, buckets(i), newNode, bucketsMin(i), bucketsMax(i), curDepth + 1)
        }
        i += 1
      }
    }
  }

  /**
    * Builds an mdd representing the carthesian product of the domain. This mdd has a width of 1 and
    * is a "simple linear graph"
    * @param domains
    * @return
    */
  def buildLinearMddFromDomains(domains: Array[Array[Int]]): StaticMddImpl = {
    val mdd = new StaticMddImpl(domains.length)
    var curNode = mdd.root
    var i = 0
    while (i < domains.length) {
      var dest: StaticMddNode = null
      if (i < domains.length - 1) {
        dest = new StaticMddNode(i + 1)
        mdd.layers(i + 1).add(dest)
      }
      else {
        dest = mdd.end
      }
      for (value <- domains(i)) {
        val edge = new StaticMddEdge(curNode, dest, value)
      }
      i += 1
      curNode = dest
    }
    mdd
  }


  /**
    * Build an mdd based on an finite state machine (the depth ofthemdd must also be specified).
    * @param automaton
    * @param numberOfVariables
    * @return
    */
  def buildMddFromRegular(automaton: Automaton, numberOfVariables : Int) : StaticMddImpl = {
    val mdd = new StaticMddImpl(numberOfVariables)
    // The root is the initial state
    val numStates = automaton.getNbStates
    val acceptingStates = automaton.getAcceptingStates
    val initialState = automaton.getInitialState
    val numLetters = automaton.getNbLetters
    val transition : Array[Array[Int]] = automaton.getTransitionMatrix
    val nullState = automaton.getNullState

    /**
      * 1. create all states below and keep in array
      * 2. for all current, add the outEdges and link
      * 3. go through that state array and for each node with in, add it to layer. if not present, do nothing
      */
    var currentLayer = new java.util.TreeMap[Int,StaticMddNode]()
    currentLayer.put(initialState,mdd.root)
    for(i <- 0 until numberOfVariables-1){
      val layerBelow = Array.fill(numStates)(new StaticMddNode(i+1))
      val keyIterator = currentLayer.keySet().iterator()
      while(keyIterator.hasNext){
        val stateVal : Int = keyIterator.next()
        val stateNode = currentLayer.get(stateVal)
        for(letter <- 0 until numLetters){
          val nextState = transition(stateVal)(letter)
          if(nextState!=nullState){
            val newMddEdge = new StaticMddEdge(stateNode,layerBelow(nextState),letter)
          }
        }
      }
      /** Final step, add what's needed**/
      val newCurLayer = new util.TreeMap[Int, StaticMddNode]()
      for(stateVal <- 0 until numStates){
        if(layerBelow(stateVal).getInSize() != 0){
          mdd.layers(i+1).add(layerBelow(stateVal))
          newCurLayer.put(stateVal,layerBelow(stateVal))
        }
      }
      currentLayer = newCurLayer
    }

    /**
      * For each current state
      * For each letter
      * If destState in set, add it, otherwise do not add edge
      *
      * At the end, go through all states of previous layer, add as deadend if no outEdge
      */
    val stateIterator = currentLayer.keySet().iterator()
    while(stateIterator.hasNext){
      val stateVal  = stateIterator.next()
      val node = currentLayer.get(stateVal)
      for(letter <- 0 until numLetters){
        val dest = transition(stateVal)(letter)
        if(acceptingStates.contains(dest)){
          // Add the edge
          val newEdge = new StaticMddEdge(node,mdd.end,letter)
        }
      }
      // If no edge lead to final state, add it to deadEnds
      if(node.getOutSize() == 0){
        mdd.pushDeadEnd(node)
      }
    }
    // At the end, clear the deadends and nodes without any in-edges
    mdd.deleteEdgesAndPropagate()
    /**
      * Optional reduction at the end to limit the size of the final mdd
      */
    mdd.reduce()
    mdd
  }

  def intersect(mdd1 : StaticMddImpl, mdd2 : StaticMddImpl) : StaticMddImpl = {
    val intersection = new StaticMddImpl(mdd1.getArity())
    if(mdd1.getArity() == mdd2.getArity()){
      val arity = mdd1.getArity()

      val nodeMap = new util.HashMap[String,StaticMddNode]()
      val mapNode1 = new util.HashMap[StaticMddNode,StaticMddNode]()
      val mapNode2 = new util.HashMap[StaticMddNode,StaticMddNode]()
      mapNode1.put(intersection.root,mdd1.root)
      mapNode2.put(intersection.root,mdd2.root)

      for(i <- 0 until arity){
        val layer = intersection.layers(i)

        val layerIterator = layer.iterator()


        while(layerIterator.hasNext){
          val u : StaticMddNode = layerIterator.next()
          val u1 : StaticMddNode = mapNode1.get(u)
          val u2 : StaticMddNode = mapNode2.get(u)
          // For each out edge of v1, check if there is a matching out outedge of v2
          val u1EdgesIterator = u1.getOutEdgeIterator()
          while(u1EdgesIterator.hasNext){
            val u1Out : StaticMddEdge = u1EdgesIterator.next()
            val value : Int = u1Out.value

            val u2Out : StaticMddEdge = u2.getOutEdge(value)
            if(u2Out != null){
              val v1 = u1Out.bottomNode
              val v2 = u2Out.bottomNode
              val v1Id = v1.getId()
              val v2Id = v2.getId()

              var v : StaticMddNode = nodeMap.get(s"$v1Id#$v2Id")
              if(v == null){
                if(i == arity-1){
                  v = intersection.end
                }
                else{
                  v = new StaticMddNode(i+1)
                  intersection.layers(i+1).add(v)
                }
                nodeMap.put(s"$v1Id#$v2Id",v)
                mapNode1.put(v,v1)
                mapNode2.put(v,v2)

              }
              // Now add the arc
              val edge = new StaticMddEdge(u,v,value)
            }
          }

          if(u.getOutSize() == 0){
            intersection.pushDeadEnd(u)
          }
        }
      }
      intersection.deleteEdgesAndPropagate()
      intersection.reduce()
    }
    return intersection
  }
}