package oscar.cbls.lib.invariant.logic

import oscar.cbls.IntValue
import oscar.cbls.algo.quick.QList
import oscar.cbls.core.computation.{ChangingIntValue, IntInvariant, IntNotificationTarget}
import oscar.cbls.core.propagation.Checker

//the output is the first indice i in the table such that for each j, table(i)(j) >= variables(j)
//default if no such line exist
class ParetoTable(val variables:Array[IntValue],
                  tables:Array[Array[Long]], //should be incomparable rows otherwise it is pointless
                  defaultIfNoDominate:Long)
  extends IntInvariant with IntNotificationTarget{
  val t = tables.length
  val d = variables.length
  for(t <- tables) require(t.length == d)

  registerStaticAndDynamicDependencyArrayIndex(variables)
  this.finishInitialization()

  override def toString: String = {
    s"""ParetoTable(
       |  variables:${variables.map(_.value).mkString(",")}
       |  tables:
       |    ${tables.map(_.mkString(",")).zipWithIndex.map(x => if(x._2 == this.value) x._1 + "<--" else x._1).mkString("\n\t\t")}
       |  defaultIfNoDominate:$defaultIfNoDominate)""".stripMargin
  }

  val smallestAmongAllRows = Array.tabulate(d)(i => tables.map(row => row(i)).min)
  val dimensionList = QList.buildFromIterable(0 until d)

  var exploreFrom:Int = -1
  override def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit = {
    //a set of possible schedules:
    //restart
    //something was increased, but still fits the current line, => no propagate
    //something has increased and does not fit anymore on the current line => schedule,start from old
    //something has decreased: start from scratch

    //startFromScratch is represented by exploreFrom set to -1
    //otherwise it is the line where we start from, excluded
    //this basically is the same

    if(isScheduled) {
      if (newVal < oldVal)
        exploreFrom = -1
      return
    }
    if(newVal > oldVal && exploreFrom != -1){
      //it has increased and there is a chance to set a starting point
      if(tables(this.newValue.toInt)(id) >= newVal) {
        //current line can still accommodate this new value, so nothing to do
      }else{
        //current line cannot accommodate this change, so we schedule for propagation,
        // but still, we can start at the current line for the exploration
        this.scheduleForPropagation()
      }
    }else{
      //it decreases, so we schedule and forget the startFrom
      this.scheduleForPropagation()
      exploreFrom = -1
    }
  }

  override def performInvariantPropagation(): Unit = {
    val a = searchFromScratchLin(variables.map(_.value),exploreFrom)
    this := (if (a == -1) defaultIfNoDominate else a)
    exploreFrom = a
  }

  def searchFromScratchLin(v:Array[Long], staAt:Int):Int = {
    var i:Int = (if(staAt == -1) 0 else staAt)
    //if the smallest is bigger or equal to the value, we do not need to check it.
    //this is a speedup when there are many dimensions, with many zero's as we expect here.
    val relevantDimensions:QList[Int] = QList.qFilter(dimensionList,i => smallestAmongAllRows(i) < v(i))

    while(i < t){

      if(dominates(tables(i),v,relevantDimensions)) return i
      i = i+1
    }

    -1
  }

  def dominates(x:Array[Long], y:Array[Long], relevantDimensions:QList[Int]):Boolean = {
    var d = relevantDimensions
    while(d != null){
      val i = d.head
      if(x(i) < y(i)) return false
      d = d.tail
    }
    true
  }

  /** To override whenever possible to spot errors in invariants.
   * this will be called for each invariant after propagation is performed.
   * It requires that the Model is instantiated with the variable debug set to true.
   */
  override def checkInternals(c: Checker): Unit ={
    val a = searchFromScratchLin(variables.map(_.value),-1)
    require(this.value == (if (a == -1) defaultIfNoDominate else a),
      s"For variables :${variables.mkString(";")}. It should be ${if (a == -1) defaultIfNoDominate else a}(from scratch) but it is ${this.value} (Incremental)")
  }
}
