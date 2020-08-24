package com.example.cbls


class Store{
  def getSolution:Solution = {
    new Solution
  }

  def getIntVar(id:Int):Variable = ???
  def getSetVar(id:Int):Variable = ???
  def getSeqVar(id:Int):Variable = ???
  def getChangingIntValue(id:Int):Variable = ???
}

class PropagationElement{
  def getUniqueId:Int = ???
}
class ChangingIntValue() extends PropagationElement{

}

class Variable() extends PropagationElement{
  def saveSolution:SolutionElement = ???
}

class SolutionElement{
  def reload(m:Store):Unit = ???
}

//Il faut modificer Solution pour que ce soit sérialisable. Je dois donc ajouter le store à l'opération load.
class Solution{
  def load(m:Store) = {
  }
}

// ////////////////////////////////////////////////////////////

class Objective{
  def getIndependentObj:IndependentOBj = ???
}

abstract class IndependentOBj{
  def convertToOBj(m:Store):Objective
}

// ////////////////////////////////////////////////////////////

object Acceptation{
  def reduce:Reduce = new Reduce()
  def lowerThan(v:Long) = new LowerThan(v:Long)
  def lowerThanMetropolis(v:Long) = new LowerThanMetropolis(v:Long)
}

abstract class Acceptation{
  def isAccepted(before:Long,after:Long):Boolean
}

case class Reduce() extends Acceptation{
  override def isAccepted(before: Long, after: Long): Boolean = after < before
}

case class LowerThan(v:Long) extends Acceptation{
  override def isAccepted(before: Long, after: Long): Boolean = after < v
}

case class LowerThanMetropolis(v:Long) extends Acceptation{
  override def isAccepted(before: Long, after: Long): Boolean = after < v
}

// ////////////////////////////////////////////////////////////
case class RemoteNeighborhoodIdentification(neighborhoodID:Long, parameters:List[Long], neighborhoodName:String)

abstract class RemoteNeighborhood(){
  def explore(parameters:List[Long],obj:Objective,acc:Acceptation,shouldAbort:() => Boolean):SearchResult
}

// ////////////////////////////////////////////////////////////

abstract class SearchResult
case class MoveFound(i:Move) extends SearchResult
case class NoMoveFound() extends SearchResult

// ////////////////////////////////////////////////////////////

abstract class IndependentSearchResult{
  def getLocalResult(m:Store):SearchResult
}
case class IndependentMoveFound(i:IndependentMove) extends IndependentSearchResult{
  override def getLocalResult(m: Store): SearchResult = MoveFound(new MoveWrapper(i,m))
}
case class IndependentNoMoveFound() extends IndependentSearchResult{
  override def getLocalResult(m: Store): SearchResult = NoMoveFound()
}

// ////////////////////////////////////////////////////////////

trait IndependentMove{
  def commit(m:Store): Unit
}

case class LoadIndependentSolutionMove(s:Solution) extends IndependentMove{
  def commit(m:Store): Unit = {
    s.load(m)
  }
}

abstract class Move{
  def commit()
  def getIndependentMove(m:Store):IndependentMove = {
    println(s"move ${this.getClass.getName} uses default getIndependentMove; dedicated implementation would be faster")
    val s = m.getSolution
    this.commit()
    val x = LoadIndependentSolutionMove(m.getSolution)
    s.load(m) //not sure if this is useful at all.
    x
  }
}

class MoveWrapper(i:IndependentMove,m:Store) extends Move{
  override def commit(): Unit = i.commit(m)

  override def toString: String = s"MoveWrapper($i)"
}