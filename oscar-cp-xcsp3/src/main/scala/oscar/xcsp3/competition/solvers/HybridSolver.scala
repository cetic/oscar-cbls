package oscar.xcsp3.competition.solvers

import oscar.algo.Inconsistency
import oscar.algo.search.{Branching, DFSearch}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSBuilder
import oscar.cp.searches.lns.operators.SearchFunctions._
import oscar.cp.searches.lns.search.{ALNSConfig, ALNSSearch, ALNSSearchResults}
import oscar.cp.{CPSolver, NoSolutionException, conflictOrderingSearch, learnValueHeuristic}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2
import oscar.xcsp3.competition.{CompetitionApp, CompetitionConf}

import scala.collection.mutable

/**
  * This Hybrid solver uses a mix of complete search and ALNS search.
  */
object HybridSolver extends CompetitionApp with App {

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

    if (parsingResult.isDefined) {
      val (decisionVars, auxiliaryVars, solver, solutionGenerator) = parsingResult.get
      val vars = decisionVars ++ auxiliaryVars
      solver.silent = true

      val maximizeObjective: Option[Boolean] = if(solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None
      var optimumFound = false

      val timeout = ((conf.timelimit() -5).toLong * 1000000000L) - (System.nanoTime() - tstart)
      var endSearch: Long = System.nanoTime() + (if(maximizeObjective.isDefined) (timeout * 0.1).toLong else timeout)
      val endTime = System.nanoTime() + timeout
      var lastSolTime = 0L

      val sols = mutable.ListBuffer[(CPIntSol, String)]()
      solver.onSolution{
        val time = System.nanoTime() - startTime
        lastSolTime = time
        val sol = new CPIntSol(vars.map(_.value), if (maximizeObjective.isDefined) solver.objective.objs.head.best else 0, time)
        val instantiation = solutionGenerator()
        optimumFound = if (maximizeObjective.isDefined) solver.objective.isOptimum() else true //In case of CSP, no point of searching another solution
        if(sols.isEmpty || (maximizeObjective.isDefined && ((maximizeObjective.get && sol.objective > sols.last._1.objective) || (!maximizeObjective.get && sol.objective < sols.last._1.objective)))){
          updateSol(instantiation, sol.objective, maximizeObjective.isDefined)
          sols += ((sol, instantiation))
        }
      }

      /**
        * Stop condition of the first complete search.
        * It's goal is to find and prove quickly the optimum on small instances and to get a good first solution for
        * the alns on larger instances.
        */
      var stopCondition = (_: DFSearch) => {
        val now = System.nanoTime()
        var stop = false
        //We stop if:
        stop |= now >= endTime //Total time used
        stop |= sols.nonEmpty && now >= endSearch //At least one solution found and search time used
        stop |= optimumFound //An optimum has been found
        stop |= sols.nonEmpty && (now - startTime) > (lastSolTime * 2L) //At least one solution has been found and more than twice time has been used since then
        stop
      }

      printComment("Parsing done, starting first complete search...")

      var stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
        solver.search(
          if(auxiliaryVars.isEmpty) conflictOrderingSearch(
            decisionVars,
            i => decisionVars(i).size,
            learnValueHeuristic(decisionVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) decisionVars(_).min else decisionVars(_).max else decisionVars(_).max)
          )
          else conflictOrderingSearch(
            decisionVars,
            i => decisionVars(i).size,
            learnValueHeuristic(decisionVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) decisionVars(_).min else decisionVars(_).max else decisionVars(_).max)
          ) ++ conflictOrderingSearch(
            auxiliaryVars,
            i => auxiliaryVars(i).size,
            learnValueHeuristic(auxiliaryVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) auxiliaryVars(_).min else auxiliaryVars(_).max else auxiliaryVars(_).max)
          )
        )
      }

      if(!optimumFound && !stats.completed) {
        printComment("First complete search done, starting ALNS search...")

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
        val result: ALNSSearchResults = if(sols.isEmpty) alns.search() else alns.searchFrom(sols.last._1)
        optimumFound = result.optimumFound

        printComment("ALNS done, starting second complete search")

        //Selecting search function based on operator that induced the most improvement:
        val search: Branching = {
          val bestOperator = result.searchOperators.maxBy(_.improvement)
          if (bestOperator.improvement > 0) {
            printComment("Best operator: " + bestOperator.name + " with improvement of: " + bestOperator.improvement)

            val valLearn = bestOperator.name.contains("valLearn")
            val iStart = if(valLearn) bestOperator.name.lastIndexOf("valLearn") + 8 else bestOperator.name.lastIndexOf('(') + 1
            val iEnd = bestOperator.name.lastIndexOf(')')
            val valHeuris = bestOperator.name.substring(iStart, iEnd)

            if(auxiliaryVars.isEmpty){
              if (bestOperator.name.contains(ALNSBuilder.BinSplitSearch)) binarySplit(decisionVars, valHeuris, valLearn)
              else if (bestOperator.name.contains(ALNSBuilder.FirstFailSearch)) firstFail(decisionVars, valHeuris, valLearn)
              else if (bestOperator.name.contains(ALNSBuilder.LastConfSearch)) lastConflict(decisionVars, valHeuris, valLearn)
              else if (bestOperator.name.contains(ALNSBuilder.ExtOrientedSearch)) extensionalOriented(decisionVars, valHeuris, valLearn)
              else if (bestOperator.name.contains(ALNSBuilder.WeightDegSearch)) weightedDegree(decisionVars, valHeuris, 0.99)
              else conflictOrdering(decisionVars, valHeuris, valLearn)
            }
            else{
              if (bestOperator.name.contains(ALNSBuilder.BinSplitSearch)) binarySplit(decisionVars, valHeuris, valLearn) ++ binarySplit(auxiliaryVars, valHeuris, valLearn)
              else if (bestOperator.name.contains(ALNSBuilder.FirstFailSearch)) firstFail(decisionVars, valHeuris, valLearn) ++ firstFail(auxiliaryVars, valHeuris, valLearn)
              else if (bestOperator.name.contains(ALNSBuilder.LastConfSearch)) lastConflict(decisionVars, valHeuris, valLearn) ++ lastConflict(auxiliaryVars, valHeuris, valLearn)
              else if (bestOperator.name.contains(ALNSBuilder.ExtOrientedSearch)) extensionalOriented(decisionVars, valHeuris, valLearn) ++ extensionalOriented(auxiliaryVars, valHeuris, valLearn)
              else if (bestOperator.name.contains(ALNSBuilder.WeightDegSearch)) weightedDegree(decisionVars, valHeuris, 0.99) ++ weightedDegree(auxiliaryVars, valHeuris, 0.99)
              else conflictOrdering(decisionVars, valHeuris, valLearn) ++ conflictOrdering(auxiliaryVars, valHeuris, valLearn)
            }
          }
          else { //Default search: Conflict ordering with min val heuristic and no learning:
            if (auxiliaryVars.isEmpty) conflictOrdering(decisionVars, "Min", valLearn = false)
            else conflictOrdering(decisionVars, "Min", valLearn = false) ++ conflictOrdering(auxiliaryVars, "Min", valLearn = false)
          }
        }

        /**
          * Stop condition of the second complete search.
          * It's goal is to prove an eventual optimum for the last solution found by the alns search.
          */
        stopCondition = (_: DFSearch) => {
          val now = System.nanoTime()
          var stop = false
          //We stop if:
          stop |= now >= endTime //Total time used
          stop |= optimumFound //An optimum has been found
          stop
        }

        stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null) {
          solver.search(search)
        }
      }

      if (sols.nonEmpty){
        if(maximizeObjective.isDefined && (optimumFound || stats.completed)) status = "OPTIMUM FOUND"
      }
      else if (stats.completed) status = "UNSATISFIABLE"
      else printDiagnostic("NO_SOL_FOUND")
      printStatus()
    }
  }
}
