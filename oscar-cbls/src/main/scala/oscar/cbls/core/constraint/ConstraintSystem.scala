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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/
package oscar.cbls.core.constraint

import oscar.cbls.core.computation._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.numeric.{ExtendableSum, Prod, Prod2, Sum}

/** A constraint system is a composition of constraints.
 * It is itself a constraint, offering the same features, namely, a global violation and a violation specific to each variable.
 * monitoring the violation of a variable requires that the ConstraintSystem has been notified that the variable should have an associated violation degree.
 * This is achieved by calling the method registerForViolation(v:Variable).
 * @param model is the model in which all the variables referenced by the constraints are declared.
 * @author renaud.delandtsheer@cetic.be
 */
case class ConstraintSystem(model:Store) extends Constraint with Objective{
  //ConstraintSystems do not act as invariant because everything is subcontracted

  model.addToCallBeforeClose(() => this.close())

  class GlobalViolationDescriptor(val Violation:ExtendableSum){
    var AggregatedViolation:List[IntValue] = List.empty
  }

  val IndexForLocalViolationINSU = model.newStorageKey()
  val IndexForGlobalViolationINSU = model.newStorageKey()

  var Violation:ChangingIntValue = null

  private var PostedConstraints:List[(Constraint,IntValue)] = List.empty
  //private var AllVars:SortedMap[Variable,List[(Constraint,IntVar)]]=SortedMap.empty

  private var PostedInvariants:List[ChangingIntValue] = List.empty

  private var VarInConstraints:List[AbstractVariable] = List.empty
  private var VarsWatchedForViolation:List[AbstractVariable] = List.empty

  override def detailedString(short: Boolean, indent:Long = 0L): String = {
    val displayedConstraints = (if(short) violatedConstraints
      else PostedConstraints.map(_._1)).sortBy(c => c.violation.value).map(c => c + " " + "viol:" + c.violation.value)

    val displayedConstraintsString = if(displayedConstraints.isEmpty) "None" else ("\n" + nSpace(indent+4L) + displayedConstraints.mkString("\n" + nSpace(indent+4L)))
    val constraintExplanationString = if(short) "violated_constraints" else "all_constraints"

    s"ConstraintSystem(${this.Violation} $constraintExplanationString:$displayedConstraintsString)"
  }

  /**
   * @return the constraints posted in the constraint system, together with their weighting factor.
   */
  def getPostedConstraints:List[(Constraint,IntValue)] = PostedConstraints

  /**Method used to post a constraint in the constraint system. (synonym of post)
    * Cannot be called after the constraint system has been closed.
    * The violation degree of the constraint system is the weighted sum of the violation degree of the posted constraints.
    * The same weighting is used to compute the violation degree of a specific variable, as it might be involved in several constraints.
    * @param c is the posted constraint.
    * @param weight is the weight that is used in the weighted sum of the violation degrees.
    */
  def add(c:Constraint,weight:IntValue=null): Unit = post(c,weight)

  def addToViolation(i:ChangingIntValue): Unit ={
    PostedInvariants = i :: PostedInvariants
  }

  /**Method used to post a constraint in the constraint system. (synonym of add)
   * Cannot be called after the constraint system has been closed.
   * The violation degree of the constraint system is the weighted sum of the violation degree of the posted constraints.
   * The same weighting is used to compute the violation degree of a specific variable, as it might be involved in several constraints.
   * @param c is the posted constraint.
   * @param weight is the weight that is used in the weighted sum of the violation degrees.
   */
  def post(c:Constraint,weight:IntValue=null): Unit ={

    PostedConstraints = (c,weight) :: PostedConstraints

    for(variable:AbstractVariable <- c.constrainedVariables){
      val oldConstrAndWeightList:List[(Constraint,IntValue)] = variable.getStorageAt(IndexForLocalViolationINSU,List.empty)
      if (oldConstrAndWeightList.isEmpty) VarInConstraints = variable :: VarInConstraints
      variable.storeAt(IndexForLocalViolationINSU,(c,weight)::oldConstrAndWeightList)
    }
  }

  private def aggregateLocalViolations(): Unit ={
    for (variable <- VarInConstraints){
      val ConstrAndWeightList:List[(Constraint,IntValue)] = variable.getStorageAt(IndexForLocalViolationINSU,null)

      val product:List[IntValue] = ConstrAndWeightList.map((ConstrAndWeight) => {
        val constr = ConstrAndWeight._1
        val weight = ConstrAndWeight._2
        if(weight == null) constr.violation(variable)
        else Prod2(constr.violation(variable),weight)
      })

      val LocalViolation = if (product.nonEmpty && product.tail.isEmpty) product.head
                            else Sum(product)
      variable.storeAt(IndexForLocalViolationINSU,LocalViolation)
    }
  }

  private def PropagateLocalToGlobalViolations(): Unit ={
    for(varWithLocalViol <- VarInConstraints){
      val localViol:IntValue = varWithLocalViol.getAndFreeStorageAt(IndexForLocalViolationINSU)
      val sources = model.sourceVariables(varWithLocalViol)
      //TODO: this seems a bit inefficient
      for(sourcevar <- sources){
        val GlobalViol:GlobalViolationDescriptor = sourcevar.getStorageAt(IndexForGlobalViolationINSU,null)
        if (GlobalViol!=null) GlobalViol.AggregatedViolation = localViol :: GlobalViol.AggregatedViolation
      }
    }
  }

  private def aggregateGlobalViolations(): Unit ={
    for (variable <- VarsWatchedForViolation){
      val ElementsAndViol:GlobalViolationDescriptor = variable.getStorageAt(IndexForGlobalViolationINSU)
      ElementsAndViol.Violation.addTerms(ElementsAndViol.AggregatedViolation)
      ElementsAndViol.AggregatedViolation = null
    }
  }

  var isClosed = false
  /**Must be invoked before the violation can be queried.
   * no constraint can be added after this method has been called.
   * this method must also be called before closing the model.
   */
  def close(): Unit ={
    if(!isClosed){
      isClosed = true
      Violation = new Sum(PostedConstraints.map((constraintANDintvar) => {
        if(constraintANDintvar._2 == null) constraintANDintvar._1.violation
        else Prod(List(constraintANDintvar._1.violation,constraintANDintvar._2))
      }) ::: PostedInvariants).setName("violation")
      if(Violation.model==null){
        assert(PostedConstraints.size==0L,"Null model but has constraints")
        Violation.model = model
      }
      model.registerForPartialPropagation(Violation)

      if(VarsWatchedForViolation.nonEmpty) {
        aggregateLocalViolations()
        PropagateLocalToGlobalViolations()
        aggregateGlobalViolations()
      }
    }
  }
  
  /**The degree of violation associated with the variable v.
   * The constraint system must have been closed prior to calling this method.
   * @param v must have been previously declared through the registerForViolation(v:Variable) method
   */
  override def violation(v:Value):IntValue= {
    v match {
      case a: AbstractVariable =>
        val CPStoredRecord: GlobalViolationDescriptor = a.getStorageAt(IndexForGlobalViolationINSU, null)
        if (CPStoredRecord == null) {
          if (model.isClosed) throw new Exception("cannot create new violation after model is closed.")
          //not registered yet
          VarsWatchedForViolation = a :: VarsWatchedForViolation
          val violationVariable = new ExtendableSum(model, Domain(0L ,Long.MaxValue))
          violationVariable.setName("global violation of " + a.name)
          a.storeAt(IndexForGlobalViolationINSU, new GlobalViolationDescriptor(violationVariable))
          registerConstrainedVariable(v)
          violationVariable
        } else {
          //already registered
          CPStoredRecord.Violation
        }
      case _ => 0L
    }
  }

  def violations[V<:Value](vs:Array[V]):Array[IntValue] = {
    Array.tabulate(vs.length)(i => violation(vs(i)))
  }

  /**Returns the global violation of the constraint system, that is the weighted sum of the violation of the posted constraints
   *close() should have been called prior to calling this method.
   */
  override def violation:IntValue = Violation

  /** to get the violated constraints, for debugging purpose
    * @return the constraints that are violated, and whose ponderation factor is not zero
    */
  def violatedConstraints:List[Constraint] =
    PostedConstraints.filter(p => (p._2 == null || p._2.value !=0L) && !p._1.isTrue).map(p => p._1)

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {}

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  override def value: Long = Violation.value
}
