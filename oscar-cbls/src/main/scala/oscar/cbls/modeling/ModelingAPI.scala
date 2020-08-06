package oscar.cbls.modeling

import oscar.cbls.lib.search.combinators._

/**
 * Created by rdl on 11-09-17.
 */
trait ModelingAPI
  extends Constraints
  //invariants
  with LogicInvariants
  with MinMaxInvariants
  with NumericInvariants
  with SetInvariants
  with SeqInvariants
  //combinators
  with BasicCombinators
  with CompositionCombinators
  with InstrumentNeighborhoodsCombinator
  with MetaheuristicCombinators
  with NeighborhoodSelectionCombinators
  with UtilityCombinators
  //standard neighborhoods
  with StandardNeighborhoods
