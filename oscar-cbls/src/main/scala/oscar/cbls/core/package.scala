package oscar.cbls

/**
 * Created by rdl on 11-09-17.
 */
package object core {

  type Invariant = oscar.cbls.core.computation.Invariant
  val InvariantHelper = oscar.cbls.core.computation.InvariantHelper

  val DomainHelper = oscar.cbls.core.computation.DomainHelper

  type Checker = oscar.cbls.core.propagation.Checker

  type VaryingDependencies = oscar.cbls.core.computation.VaryingDependencies
  type KeyForElementRemoval = oscar.cbls.core.propagation.KeyForElementRemoval

  type Bulked[A<: Value,B] = oscar.cbls.core.computation.Bulked[A,B]

  //Long types
  type IntInvariant = oscar.cbls.core.computation.IntInvariant
  type ChangingIntValue = oscar.cbls.core.computation.ChangingIntValue
  type IntNotificationTarget = oscar.cbls.core.computation.IntNotificationTarget
  type ShortIntNotificationTarget = oscar.cbls.core.computation.ShortIntNotificationTarget

  //set types
  type SetInvariant = oscar.cbls.core.computation.SetInvariant
  type ChangingSetValue = oscar.cbls.core.computation.ChangingSetValue
  type SetNotificationTarget = oscar.cbls.core.computation.SetNotificationTarget
  type ValueWiseKey = oscar.cbls.core.computation.ValueWiseKey

  //sequence types
  type SeqInvariant = oscar.cbls.core.computation.SeqInvariant
  type SeqNotificationTarget = oscar.cbls.core.computation.SeqNotificationTarget
  type ChangingSeqValue = oscar.cbls.core.computation.ChangingSeqValue

  type SeqCheckpointedValueStack[T] = oscar.cbls.core.computation.SeqCheckpointedValueStack[T]

  type SeqUpdate = oscar.cbls.core.computation.SeqUpdate

  type SeqUpdateInsert = oscar.cbls.core.computation.SeqUpdateInsert
  val SeqUpdateInsert = oscar.cbls.core.computation.SeqUpdateInsert

  type SeqUpdateRemove = oscar.cbls.core.computation.SeqUpdateRemove
  val SeqUpdateRemove = oscar.cbls.core.computation.SeqUpdateRemove

  type SeqUpdateMove = oscar.cbls.core.computation.SeqUpdateMove
  val SeqUpdateMove = oscar.cbls.core.computation.SeqUpdateMove

  type SeqUpdateAssign = oscar.cbls.core.computation.SeqUpdateAssign
  val SeqUpdateAssign = oscar.cbls.core.computation.SeqUpdateAssign

  type SeqUpdateLastNotified = oscar.cbls.core.computation.SeqUpdateLastNotified
  val SeqUpdateLastNotified = oscar.cbls.core.computation.SeqUpdateLastNotified

  type SeqUpdateDefineCheckpoint = oscar.cbls.core.computation.SeqUpdateDefineCheckpoint
  val SeqUpdateDefineCheckpoint = oscar.cbls.core.computation.SeqUpdateDefineCheckpoint

  type SeqUpdateRollBackToCheckpoint = oscar.cbls.core.computation.SeqUpdateRollBackToCheckpoint
  val SeqUpdateRollBackToCheckpoint = oscar.cbls.core.computation.SeqUpdateRollBackToCheckpoint
}
