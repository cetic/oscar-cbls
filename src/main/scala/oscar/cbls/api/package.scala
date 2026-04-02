package oscar.cbls

package object api {
  type VRS          = oscar.cbls.modeling.routing.VRS
  type IntVariable  = oscar.cbls.core.computation.integer.IntVariable
  type IntConstant  = oscar.cbls.core.computation.integer.IntConstant
  type SetVariable  = oscar.cbls.core.computation.set.SetVariable
  type SeqVariable  = oscar.cbls.core.computation.seq.SeqVariable
  type Objective    = oscar.cbls.core.computation.objective.Objective
  type Neighborhood = oscar.cbls.core.search.Neighborhood
}
