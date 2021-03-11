package oscar.xcsp3.competition.solvers

import oscar.algo.Inconsistency
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch}
import oscar.cp.{CPSolver, NoSolutionException}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

object ALNSSolver extends CompetitionApp with App {

  override def runSolver(conf: CompetitionConf): Unit = {
    val startTime = System.nanoTime()

    val md = new ModelDeclaration

    //Parsing the instance
    printComment("Parsing instance...")
    val parsingResult = try {
      val (decisionVars, auxiliaryVars, solutionGenerator) = XCSP3Parser2.parse2(md, conf.benchname())

      val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
      md.setCurrentModel(model)

      val cpDecisionVars: Array[CPIntVar] = decisionVars.map(model.getRepresentative(_).realCPVar)
      val cpAuxiliaryVars: Array[CPIntVar] = auxiliaryVars.map(model.getRepresentative(_).realCPVar)
      val solver: CPSolver = model.cpSolver

      Some(cpDecisionVars, cpAuxiliaryVars, solver, solutionGenerator)
    } catch {
      case _: NotImplementedError =>
        status = "UNSUPPORTED"
        printStatus()
        None

      case _: NoSolutionException =>
        status = "UNSATISFIABLE"
        printStatus()
        None

      case _: Inconsistency =>
        status = "UNSATISFIABLE"
        printStatus()
        None
    }

    if (parsingResult.isDefined){
      val (decisionVars, auxiliaryVars, solver, solutionGenerator) = parsingResult.get
      val vars = decisionVars ++ auxiliaryVars
      solver.silent = true

      val timeout = ((conf.timelimit() -5).toLong * 1000000000L) - (System.nanoTime() - tstart)

      val maximizeObjective: Option[Boolean] = if(solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None

      val sols = mutable.ListBuffer[(CPIntSol, String)]()
      solver.onSolution{
        val time = System.nanoTime() - startTime
        val sol = new CPIntSol(vars.map(_.value), if (maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
        val instantiation = solutionGenerator()
        if(sols.isEmpty || (maximizeObjective.isDefined && ((maximizeObjective.get && sol.objective > sols.last._1.objective) || (!maximizeObjective.get && sol.objective < sols.last._1.objective)))){
          updateSol(instantiation, sol.objective, maximizeObjective.isDefined)
          sols += ((sol, instantiation))
        }
      }

      val builder = new ALNSBuilder(
        solver,
        decisionVars,
        auxiliaryVars,
        Array(ALNSBuilder.RandomRelax, ALNSBuilder.KSuccessiveRelax, ALNSBuilder.CircuitKoptRelax, ALNSBuilder.PropGuidedRelax, ALNSBuilder.RevPropGuidedRelax, ALNSBuilder.RandomValGroupsRelax, ALNSBuilder.MaxValRelax, ALNSBuilder.PrecedencyRelax, ALNSBuilder.CostImpactRelax, ALNSBuilder.FullRelax),
        Array(ALNSBuilder.ConfOrderSearch, ALNSBuilder.FirstFailSearch, ALNSBuilder.LastConfSearch, ALNSBuilder.ExtOrientedSearch, ALNSBuilder.WeightDegSearch),
        ALNSBuilder.RWheel,
        ALNSBuilder.LastImprovRatio,
        ALNSBuilder.RWheel,
        ALNSBuilder.LastImprovRatio,
        Array(0.1, 0.3, 0.7),
        Array(50, 500, 5000),
        true,
        false
      )

      lazy val searchStore = builder.instantiateOperatorStore(builder.instantiateFixedSearchOperators, 1.0)
      lazy val relaxStore = builder.instantiateOperatorStore(builder.instantiateFixedRelaxOperators, 1.0)

      val config = new ALNSConfig(
        relaxStore,
        searchStore,
        timeout,
        None,
        conf.memlimit(),
        "evalWindowLaborie",
        Map(Symbol("quickStart") -> true)
      )

      val alns = ALNSSearch(solver, decisionVars, auxiliaryVars, config)

      printComment("Parsing done, starting search...")

      val result = alns.search()

      if (sols.nonEmpty){
        if(solver.objective.objs.nonEmpty && result.optimumFound) status = "OPTIMUM FOUND"
      }
      else if(result.unsat) status = "UNSATISFIABLE"
      else printDiagnostic("NO_SOL_FOUND")
      printStatus()
    }
  }
}
