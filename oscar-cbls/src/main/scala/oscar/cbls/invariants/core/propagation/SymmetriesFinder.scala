package oscar.cbls.invariants.core.propagation

import oscar.cbls.invariants.core.computation.Store

import scala.collection.mutable

class SymmetriesFinder(store: Store) {

    private val buffer = new Buffer(store)

    /**
     * Detects some of the symmetries between vars with regard to p
     * @param vars the variables between which symmetries should be detected
     * @param p a PropagationElement, typically an objective or a constraint
     * @return some of the symmetries between vars with regard to p
     */
    def getSymmetries(vars:Array[PropagationElement], p:PropagationElement) : Int => Int = {
      val syms = Array.fill(vars.size){Int.MinValue}
      val symClasses = findSymmetries(p)
      for(i <- 0 until vars.size){
        syms(i) = symClasses.searchClass(vars(i))
      }
      syms.apply
    }

    /**
     * Detects variable symmetries with regard to the propagationElement p
     * @param p a propagationElement
     * @return detected symmetries, in terms of decision variables
     */
    private def findSymmetries(p: PropagationElement) : SymmetryClasses = {
      val known = buffer getKnownSymmetries p
      if( known != null){
        return known
      }
      val dependencies = p.getStaticallyListenedElements
      var syms = new SymmetryClasses()
      if(dependencies.isEmpty){
        syms.addPropagationElement(p, 1)
      }
      else{
        for(d <- dependencies){
          if(syms.nextClassId == 1) {
            syms = findSymmetries(d)
          }
          else {
            syms = syms restrict findSymmetries(d)
          }
        }
        if(p.definesSymmetries){
          syms = prospectForNewSymmetries(syms, p.localSymmetries)
        }
      }
      buffer.addToKnownSymmetries(p, syms)
      syms
    }

    /**
     * Returns the symmetry classes generated by adding local symmetries to already transmitted global symmetries
     * @param globalSyms already transmitted global symmetries
     * @param localSyms local symmetries
     * @return  symmetry classes generated by adding local symmetries to already transmitted global symmetries
     */
    private def prospectForNewSymmetries(globalSyms: SymmetryClasses, localSyms : SymmetryClasses) : SymmetryClasses = {
      val candidates : List[(PropagationElement, Int)] = globalSyms.getOneBySymmetryClass
      val symmetryFound = Array.fill(candidates.size){false}
      var syms = globalSyms
      for(i <- 0 until candidates.size -1 ){
        for(j <- i + 1 until candidates.size){
          if(! symmetryFound(j) && symmetryHolds(localSyms, candidates(i)._1, candidates(j)._1)){
            syms = syms.fuse(candidates(i)._2, candidates(j)._2)
            symmetryFound(j) = true
          }
        }
      }
      syms
    }

    /**
     * Checks if a suspected global symmetry between p1 and p2 does exist
     * @param localSyms local symmetries of the propagationElement whose suspected global symmetry comes from
     * @param p1 a propagationElement, suspected to be symmetric with p2
     * @param p2 a propagationElement, suspected to be symmetric with p1
     * @return true if the symmetry holds, false otherwise
     */
    private def symmetryHolds(localSyms: SymmetryClasses, p1:PropagationElement, p2: PropagationElement) : Boolean = {
      val symClasses : Array[mutable.MutableList[PropagationElement]] = localSyms.getSymmetryClasses
      for(symClass <- symClasses){
        val toMatch = symClass
        var leftToMatch = List.empty[PropagationElement]
        for(pToMatch <- toMatch){
          val knownSyms = buffer.getKnownSymmetries(pToMatch)
          if(knownSyms.searchClass(p1) != knownSyms.searchClass(p2)){
            leftToMatch = pToMatch :: leftToMatch
          }
        }
        if(leftToMatch.size % 2 != 0) {
          return false
        }
        val paired = Array.fill(leftToMatch.size){false}
        for(i <- 0 until leftToMatch.size - 1) {
          for (j <- i + 1 until leftToMatch.size) {
            if (!paired(i) && !paired(j) && compareTrees(leftToMatch(i), leftToMatch(j), p1, p2)) {
              paired(j) = true
              paired(i) = true
            }
          }
          if(! paired(i) ){
            return false
          }
        }
        if( leftToMatch.size > 0 && ! paired(leftToMatch.size - 1 ) ){
          return false
        }
      }
      true
    }

    /**
     * Returns true if the trees t1 and t2 represent the same relations, under permutation of p1 and p2
     * @param t1  first tree to compare
     * @param t2  second tree to compare
     * @param p1  first propagationElement of the supposed permutation
     * @param p2  second propagationElement of the supposed permutation
     * @return true if the trees represent the same relations under permutation of p1 and p2, false otherwise
     */
    private def compareTrees(t1: PropagationElement, t2: PropagationElement, p1:PropagationElement, p2: PropagationElement) : Boolean = {
      var dependenciest1 = t1.getStaticallyListenedElements
      var dependenciest2 = t2.getStaticallyListenedElements
      if (dependenciest1.isEmpty && dependenciest2.isEmpty) {                       //Si ce sont des variables décisionnelles
        if (t1 != p1 && t1 != p2 && t2 != p1 && t2 != p2 && t1 == t2) {             //Si ce ne sont pas les deux variables permutables, elles doivent être les mêmes
          return true
        }
        else if (t1 == p1 && t2 == p2) {                                            //Si ce sont les variables permutables, elles doivent être différentes (1)
          return true
        }
        else if (t1 == p2 && t2 == p1) {                                             //Si ce sont les variables permutables, elles doivent être différentes (2)
          return true
        }
        else {                                                                      //Ce sont des variables différentes, mais pas celles permutables
          return false
        }
      }
      else if (dependenciest1.size != dependenciest2.size) {                        //Si le nombre de dépendances est différents, les arbres sont différents
        return false
      }
      else if (t1.allInputsExplicits && t2.allInputsExplicits && t1.getClass == t2.getClass){   //Si les invariants sont comparables et de même type
        for(i <- 0 until dependenciest1.size){
          val t1Head = dependenciest1.head
          dependenciest1 = dependenciest1.tail

          val t2Head = dependenciest2.head
          dependenciest2 = dependenciest2.tail

          if(!compareTrees(t1Head, t2Head, p1, p2)){                                            //On compare tous les sous-arbres
            return false
          }
        }
        return true
      }
      false
    }
  }

/**
 * Datastruct : set of symmetry classes of a propagationElement
 */
  class SymmetryClasses() {

    val elements : mutable.SortedSet[(PropagationElement, Int)] = mutable.SortedSet.empty[(PropagationElement, Int)](Ordering.by(_._1))
    var nextClassId : Int = 1

  /**
   * Adds a PropagationElement to the given symmetry class. classId must be such that 0 < classId < nextClassId + 1
   * @param p the PropagationElement to add
   * @param classId the symmetry class of p
   */
    def addPropagationElement(p:PropagationElement, classId:Int) = {
      if(classId > nextClassId || classId < 1) {
        throw new Exception("Invalid class Id")
      }
      if(p == null){
        throw new Exception("Trying to set a null PropagationElement")
      }
      if(classId == nextClassId){
        nextClassId += 1
      }
      elements += ((p, classId))
    }

  /**
   * Computes the symmetry classes given by the intersections of the symmetry classes of two relations
   * @param other the symmetry classes of a second relation
   * @return symmetry classes given by the intersections of the symmetry classes of the two relations
   */
    def restrict(other:SymmetryClasses) : SymmetryClasses = {
      val syms = new SymmetryClasses()
      var c1 = elements
      var c2 = other.elements
      val convertId = Array.fill(nextClassId * other.nextClassId){-1}
      while(c1.nonEmpty || c2.nonEmpty){
        var cp : Int = 0
        var toAdd : PropagationElement = null
        if(c1.nonEmpty && c2.nonEmpty) {
          val c1h = c1.head
          val c2h = c2.head
          if (c1h._1 == c2h._1) {
            cp = c1h._2 + nextClassId * c2h._2
            toAdd = c1h._1
            c1 = c1.tail
            c2 = c2.tail
          }
          else if (c1h._1 < c2h._1) {
            cp = c1h._2
            toAdd = c1h._1
            c1 = c1.tail
          }
          else if (c1h._1 > c2h._1) {
            cp = nextClassId * c2h._2
            toAdd = c2h._1
            c2 = c2.tail
          }
        }
        else if(c1.nonEmpty){
          val c1h = c1.head
          cp = c1h._2
          c1 = c1.tail
          toAdd = c1h._1
        }
        else if(c2.nonEmpty){
          val c2h = c2.head
          cp = nextClassId * c2h._2
          c2 = c2.tail
          toAdd = c2h._1
        }
        if(convertId(cp) == -1){
          convertId(cp) = syms.nextClassId
        }
        syms.addPropagationElement(toAdd, convertId(cp))
      }
      syms
    }

  /**
   * Computes and re-orders the symmetryClasses assuming that idClass1 and idClass2 must be grouped inside an unique symmetry class
   * @param idClass1 first idClass that has to be fused
   * @param idClass2 second idClass that has to be fused
   * @return a SymmetryClasses where the elements in the symmetry classes idClass1 and idClass2 are inside the same symmetry class
   */
    def fuse(idClass1: Int, idClass2 : Int) : SymmetryClasses = {
      val upperIdClass = Math.max(idClass1, idClass2)
      val lowerIdClass = Math.min(idClass1, idClass2)
      val fused = new SymmetryClasses()
      for(element <- elements) {
        if(element._2 == upperIdClass)  {
          fused.addPropagationElement(element._1, lowerIdClass)
        }
        else if (element._2 > upperIdClass) {
          fused.addPropagationElement(element._1, element._2 - 1)
        }
        else  {
          fused.addPropagationElement(element._1, element._2)
        }
      }
      fused
    }

  /**
   * Returns an unique PropagationElement by symmetry class, with it's symmetry class
   * @return
   */
    def getOneBySymmetryClass : List[(PropagationElement, Int)] = {
      val alreadyTaken = Array.fill(nextClassId){false}
      val l = mutable.MutableList.empty[(PropagationElement, Int)]
      for(element <- elements){
        if(! alreadyTaken(element._2)){
          alreadyTaken(element._2) = true
          l += element
        }
      }
      l.toList
    }

    /**
     * Returns all the symmetry classes inside an Array such that Array(i) contains a List of the PropagationElement whose symmetry class is i
     * @return all the PropagationElement, by symmetry class
     */
    def getSymmetryClasses : Array[mutable.MutableList[PropagationElement]] = {
      val res = Array.fill(nextClassId){mutable.MutableList.empty[PropagationElement]}
      for(element <- elements){
        res(element._2) += element._1
      }
      res
    }

  /**
   * Returns the id of the symmetry class of the given PropagationElement. If p has not been added to a symmetry class, returns 0 instead.
   * @param p
   * @return
   */
    def searchClass(p: PropagationElement): Int = {
      searchClassInList(elements.toList, p, 0, elements.size - 1)
    }

  /**
   * Performs a dichtomical search to find the symmetry class of p
   * @param lElements
   * @param p
   * @param fromIndex
   * @param toIndex
   * @return
   */
    private def searchClassInList(lElements: List[(PropagationElement, Int)], p: PropagationElement, fromIndex: Int, toIndex: Int) : Int = {
      if(fromIndex > toIndex) return 0
      val index: Int = (fromIndex + toIndex) / 2
      if (lElements(index)._1 == p) {
        if (getSymmetryClasses(lElements(index)._2).size == 1) {
          //si l'élément est seul dans sa classe de symétrie, il est linké à Int.MinInt
          return Int.MinValue
        }
        return lElements(index)._2
      }
      else if (fromIndex != toIndex) {
        if (lElements(index)._1 > p) {
          return searchClassInList(lElements, p, fromIndex, index - 1)
        }
        else {
          return searchClassInList(lElements, p, index + 1, toIndex)
        }
      }
      0                                                         //sinon,il n'est pas dans les variables spécifiées, donc symétrique avec toute autre variable non spécifiée
    }

    def println = {for(e <- elements) print(e + "; "); print("\n")}

  }

/**
 * Buffer datastruct
 * @param store
 */
class Buffer(store: Store) {
  val knownSymmetries = store.getNodeStorage[SymmetryClasses]

  /**
   * Adds the known symmetries s with regard to p to the buffer
   * @param p
   * @param s
   */
  def addToKnownSymmetries(p:PropagationElement, s:SymmetryClasses) = {
    knownSymmetries.update(p, s)
  }

  /**
   * Retrieves the known symmetries with regard to p from the buffer
   * @param p
   * @return
   */
  def getKnownSymmetries(p: PropagationElement) : SymmetryClasses =  {
    knownSymmetries.get(p)
  }
}




