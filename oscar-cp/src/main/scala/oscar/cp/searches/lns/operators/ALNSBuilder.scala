package oscar.cp.searches.lns.operators

import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.selection._

import scala.collection.mutable.ArrayBuffer

/**
  * Companion object for the ALNSBuilder class. Contains the keys and default values for the ALNS elements
  * supported by the ALNSBuilder.
  */
object ALNSBuilder{

  /**
    * Available relax operators:
    */

  val RandomRelax = "RandomRelax"
  val KSuccessiveRelax = "KSuccessiveRelax"

  // Propagation guided relaxation:
  val PropGuidedRelax = "PropGuidedRelax"

  // Reversed propagation guided relaxation:
  val RevPropGuidedRelax  = "RevPropGuidedRelax"

  // Value guided relaxation:
  val RandomValGroupsRelax = "RandomValGroupsRelax"
  val MinValGroupsRelax = "MinValGroupsRelax"
  val MaxValGroupsRelax = "MaxValGroupsRelax"
  val MinValRelax = "MinValRelax"
  val MaxValRelax = "MaxValRelax"
  val MinMaxValRelax = "MinMaxValRelax"

  // Circuit relaxation:
  val CircuitSeqRelax = "CircuitSeqRelax"
  val CircuitKoptRelax = "CircuitKoptRelax"

  // Scheduling relaxation
//  val ValWindowRelax = "ValWindowRelax"
//  val DefValWindowParam1 = Array(0.5, 1.0)
//  val DefValWindowParam2 = Array(0.1, 0.5)
  val PrecedencyRelax = "PrecedencyRelax"

  //cost impact guided relaxation
  val DefCostImpactN = 1
  val DefCostImpactAlpha = 0.5
  val CostImpactRelax = "CostImpactRelax"

  // Full relaxation:
  val FullRelax = "FullRelax"

  // Default relaxation size (percentage)
  val DefRelaxParam = Array(0.02, 0.05, 0.1, 0.2, 0.4) //Percentage of the neighbourhood which is relaxed
//  val DefRelaxParam = (0.1 to 0.9 by 0.1).toArray //Percentage of the neighbourhood which is relaxed

  //TODO: implement other relaxation functions

  /**
    * Available search operators:
    */
  // Conflict ordering search:
  val ConfOrderSearch = "ConfOrderSearch"

  // First fail search:
  val FirstFailSearch = "FirstFailSearch"

  // Last conflict search:
  val LastConfSearch = "LastConfSearch"

  // Binary split search:
  val BinSplitSearch = "BinSplitSearch"

  //Extentionnal Oriented search:
  val ExtOrientedSearch = "ExtOrientedSearch"

  //Max weighted degree
  val WeightDegSearch = "WeightDegSearch"
  val DefWeigDegreeParam2 = 0.99

  // Available value Heuristic functions:
  val DefValHeuris = Array("Min", "Max", /*"Median",*/ "Random")

  //Default Backtracking:
  val DefNFailures = Array(10, 100, 1000, 10000, 0)

  //Default Discrepancy:
  val DefMaxDiscrepancy = Array(Int.MaxValue)

  //TODO: implement other search heuristics

  /**
    * Available adaptive stores:
    */
  val RWheel = "RWheel"
  val Priority = "Priority"
  val Random = "Random"

  /**
    * Available performance metrics:
    */
  val LastImprov = "LastImprov"
  val LastImprovRatio = "LastImprovRatio"
  val AvgImprov = "AvgImprov"
  val AvgImprovRatio = "AvgImprovRatio"
  val TTI = "TTI"

  /**
    * Default values:
    */
  protected val DefParamSelectKey: String = RWheel
  protected val DefParamMetricKey: String = AvgImprov
  protected val DefNoParamFailThreshold = 3
  protected val DefWithParamFailThreshold = 0
}

/**
  * Builder class for ALNS elements (operators, adaptive stores and performance metrics). The elements are built based
  * on the key(s) given when calling a method. These keys are defined in the companion object.
  */
class ALNSBuilder(
                   solver: CPSolver,
                   decisionVars: Array[CPIntVar],
                   auxiliaryVars: Array[CPIntVar],
                   relaxOperatorKeys: Array[String],
                   searchOperatorKeys: Array[String],
                   opSelectionKey: String,
                   opMetricKey:String,
                   paramSelectionKey: String,
                   paramMetricKey: String,
                   relaxSize: Array[Double],
                   nFailures: Array[Int],
                   valLearn: Boolean,
                   opDeactivation: Boolean
                 ){

  def this(
            solver: CPSolver,
            vars: Array[CPIntVar],
            relaxOperatorKeys: Array[String],
            searchOperatorKeys: Array[String],
            opSelectionKey: String,
            opMetricKey:String,
            paramSelectionKey: String,
            paramMetricKey: String,
            relaxSize: Array[Double],
            nFailures: Array[Int],
            valLearn: Boolean,
            opDeactivation: Boolean
          ){
    this(
      solver,
      vars,
      Array[CPIntVar](),
      relaxOperatorKeys,
      searchOperatorKeys,
      opSelectionKey,
      opMetricKey,
      paramSelectionKey,
      paramMetricKey,
      relaxSize,
      nFailures,
      valLearn,
      opDeactivation
    )
  }

  // lazy vals used for some operators:
  lazy val N: Int = decisionVars.length // The number of vars
  lazy val maxNeighSize: Double = decisionVars.map(x => math.log(x.size)).sum // Max neighbourhood size for propagation guided relax

  def instantiateCoupledOperators: Array[ALNSOperator] =(
    for(
      relaxKey <- relaxOperatorKeys;
      searchKey <- searchOperatorKeys
    )yield (relaxKey, searchKey)
    ).flatMap(pair => instantiateCoupledOperator(pair._1, pair._2)
  )

  def instantiateRelaxOperators: Array[ALNSOperator] =
    relaxOperatorKeys.map(x => instantiateRelaxOperator(x, paramSelectionKey, paramMetricKey))

  def instantiateSearchOperators: Array[ALNSOperator] = searchOperatorKeys.flatMap(instantiateSearchOperators(_))

  def instantiateOperatorStore(operators: Array[ALNSOperator], initWeight: Double): AdaptiveStore[ALNSOperator] =
    instantiateAdaptiveStore[ALNSOperator](opSelectionKey, operators, opMetricKey, initWeight)

  def instantiateMetric(): (ALNSElement) => Double = instantiateMetric(opMetricKey)

  /**
    * Instantiates one or more coupled operators depending on the default parameters of the relaxation and search functions.
    * @param relaxKey the relaxation function key
    * @param searchKey the search function key
    * @return an array of coupled operator (one for each possible parameter combination)
    */
  private def instantiateCoupledOperator(relaxKey: String, searchKey: String): Array[ALNSOperator] = {
    for {
      (relaxName, relaxFunction) <- instantiateRelaxFunctions(relaxKey)
      (searchName, searchFunction) <- instantiateSearchFunctions(searchKey)
      nFailures <- nFailures
      maxDiscrepancy <- ALNSBuilder.DefMaxDiscrepancy
    } yield new ALNSNoParamOperator(
      relaxName + "_" + searchName + "(" + (if (nFailures == 0) "NoFailLimit" else nFailures) + "," + (if (maxDiscrepancy == Int.MaxValue) "NoMaxDiscrepancy" else maxDiscrepancy) + ")",
      if (opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0,
      () => (
        (sol: CPIntSol) => {
          relaxFunction(sol)
          searchFunction(sol)
        },
        Some(nFailures),
        Some(maxDiscrepancy)
      )
    )
  }

  def instantiateFixedRelaxOperators: Array[ALNSOperator] = {
    relaxOperatorKeys.flatMap(relaxKey => {
      instantiateRelaxFunctions(relaxKey).map{
        case (relaxName, relaxFunction) => new ALNSNoParamOperator(
          relaxName,
          if (opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0,
          () => ((sol: CPIntSol) => relaxFunction(sol), None, None)
        )
      }
    })
  }

  def instantiateFixedSearchOperators: Array[ALNSOperator] = {
    searchOperatorKeys.flatMap(searchKey => {
      for {
        (searchName, searchFunction) <- instantiateSearchFunctions(searchKey)
        nFailures <- nFailures
        maxDiscrepancy <- ALNSBuilder.DefMaxDiscrepancy
      } yield new ALNSNoParamOperator(
        searchName + "(" + (if (nFailures == 0) "NoFailLimit" else nFailures) + "," + (if (maxDiscrepancy == Int.MaxValue) "NoMaxDiscrepancy" else maxDiscrepancy) + ")",
        if (opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0,
        () => (
          (sol: CPIntSol) => searchFunction(sol),
          Some(nFailures),
          Some(maxDiscrepancy)
        )
      )
    })
  }

  /**
    * Instantiate a loose ANLS relax operator with the given parameters.
    * @param opKey The operator key
    * @param paramSelectKey The parameter selection key (only if the operator uses adaptive parameters)
    * @param paramMetricKey The parameter metric key (only if the operator uses adaptive parameters)
    * @return an ALNSOperator object
    */
  private def instantiateRelaxOperator(
                                        opKey: String,
                                        paramSelectKey: String = ALNSBuilder.DefParamSelectKey,
                                        paramMetricKey: String = ALNSBuilder.DefParamMetricKey
                                      ): ALNSOperator = opKey match {

    case ALNSBuilder.RandomRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.RandomRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.randomRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.KSuccessiveRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.KSuccessiveRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.successiveRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.PropGuidedRelax => new ALNSSingleParamOperator[Double](
      ALNSBuilder.PropGuidedRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Double) => ((sol:CPIntSol) => RelaxationFunctions.propagationGuidedRelax(solver, decisionVars, sol: CPIntSol, param: Double), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => x * maxNeighSize)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey)
    )

    case ALNSBuilder.RevPropGuidedRelax => new ALNSSingleParamOperator[Double](
      ALNSBuilder.RevPropGuidedRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Double) => ((sol:CPIntSol) => RelaxationFunctions.reversedPropagationGuidedRelax(solver, decisionVars, sol: CPIntSol, param: Double), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => x * maxNeighSize)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey)
    )

    case ALNSBuilder.RandomValGroupsRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.RandomValGroupsRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.randomGroupsRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.MinValGroupsRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MinValGroupsRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.minGroupsRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.MaxValGroupsRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MaxValGroupsRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.maxGroupsRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.MinValRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MinValRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.minValRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.MaxValRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MaxValRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.maxValRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.MinMaxValRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MinMaxValRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.minMaxValRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.CircuitSeqRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.CircuitSeqRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.predRelaxSeqFixed(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.CircuitKoptRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.CircuitKoptRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.predRelaxKopt(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt / 2)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

//    case ALNSBuilder.ValWindowRelax => new ALNSTwoParamsOperator[Double, Double](
//      ALNSBuilder.ValWindowRelax,
//      ALNSBuilder.DefWithParamFailThreshold,
//      (param1: Double, param2: Double) => ((sol:CPIntSol) => RelaxationFunctions.valWindowRelax(solver, vars, sol: CPIntSol, param1: Double, param2:Double), None, None),
//      instantiateAdaptiveStore(
//        paramSelectKey,
//        ALNSBuilder.DefValWindowParam1.map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
//        paramMetricKey
//      ),
//      instantiateAdaptiveStore(
//        paramSelectKey,
//        ALNSBuilder.DefValWindowParam2.map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
//        paramMetricKey
//      )
//    )

    case ALNSBuilder.PrecedencyRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.PrecedencyRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.precedencyRelax(solver, decisionVars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        relaxSize
          .map(x => Math.ceil(N * x).toInt)
          .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      )
    )

    case ALNSBuilder.CostImpactRelax =>
      val costImpact = new CostImpact(solver, decisionVars, ALNSBuilder.DefCostImpactN, ALNSBuilder.DefCostImpactAlpha)
      new ALNSSingleParamOperator[Int](
        ALNSBuilder.CostImpactRelax,
        ALNSBuilder.DefWithParamFailThreshold,
        (param: Int) => ((sol:CPIntSol) => costImpact.costImpactRelax(sol: CPIntSol, param: Int), None, None),
        instantiateAdaptiveStore(
          paramSelectKey,
          relaxSize
            .map(x => Math.ceil(N * x).toInt)
            .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
          paramMetricKey
        )
      )

    case ALNSBuilder.FullRelax => new ALNSNoParamOperator(
      ALNSBuilder.FullRelax,
      if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0,
      () => (_ => (), None, None)
    )
  }

  /**
    * Instantiate loose ANLS search operators for the given keys.
    * @param opKey The operator key
    * @return an array of ALNSOperator objects
    */
  private def instantiateSearchOperators(
                                          opKey: String,
                                          paramSelectKey: String = ALNSBuilder.DefParamSelectKey,
                                          paramMetricKey: String = ALNSBuilder.DefParamMetricKey
                                        ): Array[ALNSOperator] =
    instantiateSearchFunctions(opKey).map{
      case (name, function) => new ALNSTwoParamsOperator[Int, Int](
        name,
        if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0,
        (param1: Int, param2: Int) => (function, Some(param1), Some(param2)),
        instantiateAdaptiveStore(
          paramSelectKey,
          nFailures.map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
          paramMetricKey
        ),
        instantiateAdaptiveStore(
          paramSelectKey,
          ALNSBuilder.DefMaxDiscrepancy
            .map(x => instantiateParameter(x, if(opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
          paramMetricKey
        )
      )
    }

  private def instantiateRelaxFunctions(opKey: String): Array[(String, CPIntSol => Unit)] = opKey match{

    case ALNSBuilder.RandomRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.randomRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.KSuccessiveRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.successiveRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.PropGuidedRelax =>
      relaxSize
        .map(x => maxNeighSize * x)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.propagationGuidedRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.RevPropGuidedRelax =>
      relaxSize
        .map(x => maxNeighSize * x)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.reversedPropagationGuidedRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.RandomValGroupsRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.randomGroupsRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.MinValGroupsRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.minGroupsRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.MaxValGroupsRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.maxGroupsRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.MinValRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.minValRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.MaxValRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.maxValRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.MinMaxValRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.minMaxValRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.CircuitSeqRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.predRelaxSeqFixed(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.CircuitKoptRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt / 2)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.predRelaxKopt(solver, decisionVars, sol, x))
        )

//    case ALNSBuilder.ValWindowRelax =>
//      for(
//        lr <- ALNSBuilder.DefValWindowParam1;
//        ur <- ALNSBuilder.DefValWindowParam2
//      )yield (
//        opKey + "(" + lr.toString + "," + ur.toString + ")",
//        (sol: CPIntSol) => RelaxationFunctions.valWindowRelax(solver, vars, sol, lr, ur)
//      )

    case ALNSBuilder.PrecedencyRelax =>
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.precedencyRelax(solver, decisionVars, sol, x))
        )

    case ALNSBuilder.CostImpactRelax =>
      val costImpact = new CostImpact(solver, decisionVars, ALNSBuilder.DefCostImpactN, ALNSBuilder.DefCostImpactAlpha)
      relaxSize
        .map(x => Math.ceil(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => costImpact.costImpactRelax(sol, x))
        )

    case ALNSBuilder.FullRelax => Array((opKey, _ => ()))
  }

  private def instantiateSearchFunctions(opKey: String): Array[(String, CPIntSol => Unit)] = {
    val functions = ArrayBuffer[(String, CPIntSol => Unit)]()
    ALNSBuilder.DefValHeuris.foreach(heuristic =>{
      if(valLearn && opKey != ALNSBuilder.WeightDegSearch) functions += instantiateSearchFunction(opKey, heuristic, valLearn = true)
      else functions += instantiateSearchFunction(opKey, heuristic, valLearn = false)
    })
    functions.toArray
  }

  private def instantiateSearchFunction(opKey: String, valHeuristic: String, valLearn: Boolean): (String, CPIntSol => Unit) = {
    val opName = opKey + (if(valLearn && opKey != ALNSBuilder.WeightDegSearch) "(valLearn" else "(") + valHeuristic + ")"
    if(auxiliaryVars.nonEmpty) (opName, _ => opKey match{
      case ALNSBuilder.ConfOrderSearch => SearchFunctions.conflictOrdering(decisionVars, valHeuristic, valLearn) ++ SearchFunctions.conflictOrdering(auxiliaryVars, valHeuristic, valLearn)
      case ALNSBuilder.FirstFailSearch => SearchFunctions.firstFail(decisionVars, valHeuristic, valLearn) ++ SearchFunctions.firstFail(auxiliaryVars, valHeuristic, valLearn)
      case ALNSBuilder.LastConfSearch => SearchFunctions.lastConflict(decisionVars, valHeuristic, valLearn) ++ SearchFunctions.lastConflict(auxiliaryVars, valHeuristic, valLearn)
      case ALNSBuilder.BinSplitSearch => SearchFunctions.binarySplit(decisionVars, valHeuristic, valLearn) ++ SearchFunctions.binarySplit(auxiliaryVars, valHeuristic, valLearn)
      case ALNSBuilder.ExtOrientedSearch => SearchFunctions.extensionalOriented(decisionVars, valHeuristic, valLearn) ++ SearchFunctions.extensionalOriented(auxiliaryVars, valHeuristic, valLearn)
      case ALNSBuilder.WeightDegSearch => SearchFunctions.weightedDegree(decisionVars, valHeuristic, ALNSBuilder.DefWeigDegreeParam2) ++ SearchFunctions.weightedDegree(auxiliaryVars, valHeuristic, ALNSBuilder.DefWeigDegreeParam2)
    })
    else (opName, _ => opKey match{
      case ALNSBuilder.ConfOrderSearch => SearchFunctions.conflictOrdering(decisionVars, valHeuristic, valLearn)
      case ALNSBuilder.FirstFailSearch => SearchFunctions.firstFail(decisionVars, valHeuristic, valLearn)
      case ALNSBuilder.LastConfSearch => SearchFunctions.lastConflict(decisionVars, valHeuristic, valLearn)
      case ALNSBuilder.BinSplitSearch => SearchFunctions.binarySplit(decisionVars, valHeuristic, valLearn)
      case ALNSBuilder.ExtOrientedSearch => SearchFunctions.extensionalOriented(decisionVars, valHeuristic, valLearn)
      case ALNSBuilder.WeightDegSearch => SearchFunctions.weightedDegree(decisionVars, valHeuristic, ALNSBuilder.DefWeigDegreeParam2)
    })
  }

  /**
    * Instantiate an adaptive store with the given parameters.
    * @param selectionKey the adaptive store key
    * @param elems the elements to put in the store
    * @param metricKey the performance metric used
    * @tparam T the type of the elements in the store
    * @return An AdaptiveStore
    */
  private def instantiateAdaptiveStore[T <: ALNSElement](
                                    selectionKey: String,
                                    elems: Array[T],
                                    metricKey: String,
                                    initWeight: Double = -1.0
                                  ): AdaptiveStore[T] = selectionKey match{

    case ALNSBuilder.RWheel =>
      if(initWeight > 0)
        new RouletteWheel[T](
          elems,
          Array.fill(elems.length)(initWeight),
          computeRFactor(metricKey),
          computeStoreDirection(metricKey),
          instantiateMetric(metricKey)
        )
      else
        new RouletteWheel[T](
          elems,
          computeRFactor(metricKey),
          computeStoreDirection(metricKey),
          instantiateMetric(metricKey)
        )

    case ALNSBuilder.Priority =>
      if(initWeight > 0)
        new PriorityStore[T](
          elems,
          computeRFactor(metricKey),
          computeStoreDirection(metricKey),
          instantiateMetric(metricKey)
        )
      else
        new PriorityStore[T](
          elems,
          Array.fill(elems.length)(initWeight),
          computeRFactor(metricKey),
          computeStoreDirection(metricKey),
          instantiateMetric(metricKey)
        )

    case ALNSBuilder.Random => new RandomStore[T](elems)
  }

  /**
    * Instantiates a metric function.
    * @param metricKey the key of the metric function
    * @return a metric function
    */
  private def instantiateMetric(metricKey: String): (ALNSElement) => Double = metricKey match{
    case ALNSBuilder.LastImprov => Metrics.lastImprovement
    case ALNSBuilder.LastImprovRatio => Metrics.lastImprovementRatio
    case ALNSBuilder.AvgImprov => Metrics.averageImprovement
    case ALNSBuilder.AvgImprovRatio => Metrics.averageImprovementRatio
    case ALNSBuilder.TTI => Metrics.timeToImprovement
  }

  private def instantiateParameter[T](value: T, failThreshold: Int): ALNSParameter[T] = new ALNSParameter[T](
    value,
    failThreshold
  )

  private def computeRFactor(metricKey: String): Double = metricKey match{
    case ALNSBuilder.LastImprov => 0.9
    case ALNSBuilder.LastImprovRatio => 0.9
    case ALNSBuilder.AvgImprov => 1.0
    case ALNSBuilder.AvgImprovRatio => 1.0
    case ALNSBuilder.TTI => 1.0
  }

  private def computeInitScore(metricKey: String): Double = metricKey match{
    case ALNSBuilder.LastImprov => Double.MaxValue
    case ALNSBuilder.LastImprovRatio => Double.MaxValue
    case ALNSBuilder.AvgImprov => Double.MaxValue
    case ALNSBuilder.AvgImprovRatio => Double.MaxValue
    case ALNSBuilder.TTI => 0.0
  }

  private def computeStoreDirection(metricKey: String): Boolean = metricKey match{
    case ALNSBuilder.LastImprov => false
    case ALNSBuilder.LastImprovRatio => false
    case ALNSBuilder.AvgImprov => false
    case ALNSBuilder.AvgImprovRatio => false
    case ALNSBuilder.TTI => true
  }
}
