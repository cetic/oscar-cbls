package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.operators.{ALNSNoParamOperator, ALNSOperator}
import oscar.cp.searches.lns.selection.{AdaptiveStore, RandomStore}

/**
  * Search configuration
  */
class ALNSConfig(
                  val relaxStore: AdaptiveStore[ALNSOperator], //The relax operators to use
                  val searchStore: AdaptiveStore[ALNSOperator], //The search operators to use
                  val timeout: Long = 0, //Search timeout (ns)
                  val objective: Option[Int] = None, //Objective threshold to reach
                  val memLimit: Int = 1000, //Memory limit (mb)
                  val strategy: String = "default", //The ALNS search strategy to use
                  val metaParameters: Map[Symbol, Any] = Map() //The meta parameters for the ALNS Search
                ) {

  def this(
            searchStore: AdaptiveStore[ALNSOperator],
            timeout: Long,
            objective: Option[Int],
            memLimit: Int, //Memory limit (mb)
            strategy: String,
            metaParameters: Map[Symbol, Any]
          ){
    this(
      new RandomStore[ALNSOperator](Array(new ALNSNoParamOperator("dummy", 0, () => (_ => (), None, None)))),
      searchStore,
      timeout,
      objective,
      memLimit,
      strategy,
      metaParameters
    )
  }
}
