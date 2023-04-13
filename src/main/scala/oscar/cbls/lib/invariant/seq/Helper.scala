package oscar.cbls.lib.invariant.seq

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.computation.{ChangingIntValue, ChangingSeqValue, Domain, IntInvariant, IntNotificationTarget, IntValue, SeqNotificationTarget, SeqUpdate, SeqValue}
import oscar.cbls.core.propagation.Checker

/** This is a helper to define an invariant from a Seq -> Long function.
 * Ths invariant is not incremental, so it should only be used for very simple functions.
 * it maintains output = fun(a)
 *
 * @param a the parameter of the function
 * @param fun the function to maintain, it is supposed not to listen to any variable in the model
 * @param domain the expected domain of the output
 * @author renaud.delandtsheer@cetic.be
 * */
class Seq2Int(a:SeqValue, fun:IntSequence => Long, domain:Domain)
  extends IntInvariant(fun(a.value),domain)
    with SeqNotificationTarget{

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  this := fun(a.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    this := fun(changes.newValue)
  }

  override def checkInternals(c:Checker): Unit ={
    c.check(this.value == fun(a.value), Some("output.value == fun(a.value)"))
  }
}
