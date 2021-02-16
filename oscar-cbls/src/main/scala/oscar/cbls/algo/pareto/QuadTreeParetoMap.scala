package oscar.cbls.algo.pareto

class QuadTreeParetoMap[T](nbDimensions:Int)
  extends ParetoMap[T](nbDimensions) {

  private val nSuccessors: Int = 1 << nbDimensions
  private val Successors: Range = 0 until nSuccessors
  private val NonDomSuccessors: Range = 1 until nSuccessors - 1
  private val bestSuccessor: Int = Successors.max
  private val worstSuccessor: Int = Successors.min

  /** Successor functions */

  private def opposite(kSucc: Int): Int = kSucc ^ bestSuccessor

  private def asStrong(kSucc: Int): IndexedSeq[Int] = {
    (kSucc to NonDomSuccessors.max).filter(s => (s - (s ^ kSucc)) == kSucc)
  }

  private def stronger(kSucc: Int): IndexedSeq[Int] = {
    (kSucc + 1 to NonDomSuccessors.max).filter(s => (s - (s ^ kSucc)) == kSucc)
  }

  private def asWeak(kSucc: Int): IndexedSeq[Int] = {
    val dim = opposite(kSucc)
    (NonDomSuccessors.min to kSucc).filter(s => ((s ^ dim) - s) == dim)
  }

  private def weaker(kSucc: Int): IndexedSeq[Int] = {
    val dim = opposite(kSucc)
    (NonDomSuccessors.min until kSucc).filter(s => ((s ^ dim) - s) == dim)
  }

  private class QuadTreeNode(val key:Array[Long], val value:T) {

    def toCouple:(Array[Long],T) = (key,value)

    val successors: Array[Option[QuadTreeNode]] = Array.fill(nSuccessors)(None)
    var father: Option[QuadTreeNode] = None
    var kSucc: Int = -1

    def successorsToSet: Set[QuadTreeNode] = {
      successors.flatten.toSet
    }

    def detach() : Unit = { // Cannot be applied on root
      val f = father.get
      f.successors(kSucc) = None
      father = None
      kSucc = -1
    }

    def successorship(k: Array[Long]): Int = successorship0(k, 0, 0, false, false)

    @scala.annotation.tailrec
    private def successorship0(k: Array[Long], d: Int, kSucc: Int, dom: Boolean, ndom: Boolean): Int = {
      if (d == nbDimensions) if (dom && !ndom) bestSuccessor else kSucc
      else if (k(d) < key(d)) successorship0(k, d + 1, (kSucc << 1) + 1, true, ndom)
      else if (k(d) == key(d)) successorship0(k, d + 1, kSucc << 1, dom, ndom)
      else successorship0(k, d + 1, kSucc << 1, dom, true)
    }

    override def toString: String = key.mkString("QTNode(", ", ", ")")
  }


  private var root: Option[QuadTreeNode] = None

  /**
   * tries to insert cand as a child or grandchild of subtree
   * @param cand
   * @param subR
   * @return true if it was inserted, false otherwise
   */
  private def process(cand: QuadTreeNode, subR: QuadTreeNode) :Boolean = {

    val kSucc = subR.successorship(cand.key)

    // Discard the candidate node
    if (kSucc == worstSuccessor) {
      false
    } else if (kSucc == bestSuccessor) {
      // Replace the root by the dominated node
      replace(cand, subR)
      for (son <- NonDomSuccessors if cand.successors(son).isDefined) {
        clean(cand, cand.successors(son).get)
      }
      true
    }else {
      // Dominance + Clean
      // Check dominance
      for (son <- stronger(kSucc) if subR.successors(son).isDefined) {
        val dom = checkDominance(cand, subR.successors(son).get)
        if (dom) return false // dominated wy some existing node
      }
      // Remove dominated node
      val weak = weaker(kSucc)
      for (son <- weak if subR.successors(son).isDefined) {
        clean(cand, subR.successors(son).get)
      }

      if (subR.successors(kSucc).isDefined) {
        process(cand, subR.successors(kSucc).get)
      } else {
        insert0NoCheck(cand, subR)
        true
      }
    }
  }

  /** Insert the candidate node without checking for dominance */
  private def insertNoCheck(cand: QuadTreeNode) : Unit = {
    if (root.isDefined) insert0NoCheck(cand, root.get)
    else root = Some(cand)
  }

  @scala.annotation.tailrec
  private def insert0NoCheck(cand: QuadTreeNode, root: QuadTreeNode) : Unit = {
    val kSucc = root.successorship(cand.key)
    // Recursive traversal
    if (root.successors(kSucc).isDefined) {
      insert0NoCheck(cand, root.successors(kSucc).get)
    } // Insertion
    else {
      cand.father = Some(root)
      cand.kSucc = kSucc
      root.successors(kSucc) = Some(cand)
    }
  }

  /** Check if the candidate node is dominated by some node in the tree rooted at root */

  private def checkDominance(cand: QuadTreeNode, root: QuadTreeNode): Boolean = {
    val kSucc = root.successorship(cand.key)
    // Is the candidate node dominated by the root ?
    if (kSucc == worstSuccessor) true
    // Is the candidate node dominated by a subtree ?
    else {
      val sons = asStrong(kSucc)
      for (son <- sons if root.successors(son).isDefined) {
        if (checkDominance(cand, root.successors(son).get)) return true
      }
      false // Not dominated by any node in the tree rooted at root
    }
  }

  /** Replace the root */

  private def replace(cand: QuadTreeNode, root: QuadTreeNode)  : Unit = {
    // Transplant
    if (root == this.root.get) {
      this.root = Some(cand)
    } else {
      val father = root.father.get
      father.successors(root.kSucc) = Some(cand)
      cand.father = Some(father)
      cand.kSucc = root.kSucc
    }
    // Reinsert sons
    for (son <- NonDomSuccessors if root.successors(son).isDefined) {
      reinsertIn(root.successors(son).get, this.root.get)
    }
  }

  /** Reinsert without dominance check all the subtree rooted at root in inNode */

  private def reinsertIn(root: QuadTreeNode, inNode: QuadTreeNode)  : Unit = {
    root.detach()
    for (son <- NonDomSuccessors if root.successors(son).isDefined) {
      reinsertIn(root.successors(son).get, inNode)
    }
    insert0NoCheck(root, inNode)
  }

  /** Remove nodes that are dominated by the candidate node */

  private def clean(cand: QuadTreeNode, root: QuadTreeNode) : Unit = {
    val kSucc = root.successorship(cand.key)
    // Is the root dominated by the candidate node ?
    if (kSucc == bestSuccessor) {
      val newRoot = deleteAndRepair(root)
      if (newRoot.isDefined) {
        clean(cand, newRoot.get)
      }
    } // Is the candidate node dominated by a subtree ?
    else {
      val sons = asWeak(kSucc)
      for (son <- sons if root.successors(son).isDefined) {
        clean(cand, root.successors(son).get)
      }
    }
  }

  private def deleteAndRepair(root: QuadTreeNode): Option[QuadTreeNode] = {

    // Search first son
    var son = 1
    while (root.successors(son).isEmpty && son < bestSuccessor) {
      son += 1
    }

    // First son replace its father
    if (son < bestSuccessor) {
      val newRoot = root.successors(son).get
      newRoot.detach() // prevents newRoot to be reinserted into newRoot

      // Reinsert
      if (root == this.root.get) {
        this.root = Some(newRoot)
      }
      else {
        val father = root.father.get
        father.successors(root.kSucc) = Some(newRoot)
        newRoot.father = Some(father)
        newRoot.kSucc = root.kSucc
      }
      // Reinsert sons
      for (son <- NonDomSuccessors if root.successors(son).isDefined) {
        reinsertIn(root.successors(son).get, newRoot)
      }
      Some(newRoot)
    } else {
      root.detach()
      None
    }
  }


  /**
   *
   * @param key
   * @param value
   * @return true if it was inserted, false otherwise
   */
  override def insert(key: Array[Long], value: T): Boolean = {
    val quadTreeNode = new QuadTreeNode(key,value)
    if (root.isDefined) process(quadTreeNode, root.get)
    else {
      root = Some(quadTreeNode)
      true
    }
  }

  override def content: List[(Array[Long], T)] = {
    var acc:List[(Array[Long],T)] = Nil
    def search(root: QuadTreeNode) : Unit = {
      acc = root.toCouple :: acc
      for (son <- NonDomSuccessors if root.successors(son).isDefined) {
        search(root.successors(son).get)
      }
    }
    if (root.isDefined) search(root.get)
    acc
  }

  override def foreach[U](f: ((Array[Long], T)) => U): Unit = {
    def forEach0(root: QuadTreeNode) : Unit = {
      f(root.key,root.value)
      for (son <- NonDomSuccessors if root.successors(son).isDefined) {
        forEach0(root.successors(son).get)
      }
    }
    if (root.isDefined) forEach0(root.get)
  }
}


