/** *****************************************************************************
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
  * *****************************************************************************/
/**
  * @author Gustav Björdal
  * @author Jean-Noël Monette
  */
package oscar.flatzinc.cbls

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.{Objective => CBLSObjective}
import oscar.cbls.lib.search.LinearSelectors
import oscar.cbls.util.StopWatch
import oscar.flatzinc.{Log, NoSuchConstraintException, Options}
import oscar.flatzinc.cbls.support._
import oscar.flatzinc.cp.FZCPModel
import oscar.flatzinc.model.{Constraint, Variable, _}
import oscar.flatzinc.parser.FZParser
import oscar.flatzinc.transfo.FZModelTransfo



class FZCBLSBuilder extends LinearSelectors with StopWatch {


  def solve(opts: Options): Unit = {
    startWatch()
    val log = opts.log();
    log("start")

    val useCP = opts.is("usecp")

    val fzModel = FZParser.readFlatZincModelFromFile(opts.fileName, log, false).problem;

    Helper.getCstrsByName(fzModel.constraints).map
    { case (n: String, l: List[Constraint]) => l.length + "\t" + n }.toList.sorted.foreach(log(_))

    log("Parsed. Parsing took " + getWatch + " ms")
    val cpmodel = new FZCPModel(fzModel, oscar.cp.core.CPPropagStrength.Automatic)
    //println(fzModel.variables.toList.map(v => v.domainSize))


    simplifyFlatZincModel(opts, log, useCP, fzModel, cpmodel)


    //Hack for the subcircuit constraints:
    fzModel.variables.foreach(v => if (v.isDefined && v.cstrs.exists {
                                                                       case c: subcircuit => true;
                                                                       case c: circuit => true;
                                                                       case c: inverse => true;
                                                                       case _ => false
                                                                     }) {
      v.definingConstraint.get.unsetDefinedVar(v)
    })


    findInvariants(fzModel, opts, log)

    //Hack for the subcircuit constraints:
    fzModel.variables.foreach(v => if (v.isDefined && v.cstrs.exists {
                                                                       case c: subcircuit => true;
                                                                       case c: circuit => true;
                                                                       case c: inverse => true;
                                                                       case _ => false
                                                                     }) {
      v.definingConstraint.get.unsetDefinedVar(v)
    })


    val allConstraints: List[Constraint] = fzModel.constraints.toList

    val (maybeDirConstraint, maybeSoftConstraint) = allConstraints.partition(_.definedVar.isDefined)
    log("Possibly " + maybeDirConstraint.length + " invariants.")

    val (invariants, nowMaybeSoft) = FZModelTransfo.getSortedInvariants(maybeDirConstraint)(log)
    log("Sorted " + invariants.length + " Invariants")


    val cblsmodel = new FZCBLSModel(fzModel, log, () => getWatch) //This step creates all variables!

    if (useCP) cblsmodel.useCPsolver(cpmodel)
    log("Created Model (Variables and Objective)")

    //Remove variables that are in defined neighbourhoods
    removeNeighbourhoodVariables(fzModel, cblsmodel)

    /*
    for (neighbourhood <- fzModel.neighbourhoods) {
      for (sub <- neighbourhood.subNeighbourhoods) {
        val whereCS = createLocalConstraintSystem(sub.whereConstraints, cblsmodel)
        val ensureCS = createLocalConstraintSystem(sub.ensureConstraints, cblsmodel)
        val controlledVariables = sub.getSearchVariables.map(cblsmodel.getCBLSVar(_))
        cblsmodel.addNeighbourhood(
          (o, c) => new FlatSubNeighbourhood(sub,
                                             whereCS,
                                             ensureCS,
                                             controlledVariables,
                                             o,
                                             cblsmodel),
          controlledVariables)
      }
    }*/

    for (neighbourhood <- fzModel.neighbourhoods) {
      cblsmodel.addNeighbourhood(
        (o,c) => {
          val initCS = createLocalConstraintSystem(neighbourhood.initConstraints,cblsmodel)
          val subNeighbourhoods = neighbourhood.subNeighbourhoods.map(sub =>
                                                                      {
                                                                        val whereCS = createLocalConstraintSystem(sub.whereConstraints, cblsmodel)
                        val ensureCS = createLocalConstraintSystem(sub.ensureConstraints, cblsmodel)
                        val controlledVariables = sub.getSearchVariables.map(cblsmodel.getCBLSVar(_))
                        (o:CBLSObjective, c:ConstraintSystem) => new FlatSubNeighbourhood(sub,
                                                                                          whereCS,
                                                                                          ensureCS,
                                                                                          controlledVariables,
                                                                                          o,
                                                                                          cblsmodel)
                      }
          ).toArray
          new FlatNeighbourhood(neighbourhood,
                                initCS,
                                subNeighbourhoods,
                                o,
                                cblsmodel

          )
        }
        ,neighbourhood.getSearchVariables.map(cblsmodel.getCBLSVar(_))
      )

    }

    //Gets the soft constraints by posting the implicit constraints (neighbourhoods)
    val softConstraints: List[Constraint] = findAndPostImplicitConstraints(cblsmodel,
                                                                           maybeSoftConstraint ++ nowMaybeSoft, opts,
                                                                           log)


    val poster: FZCBLSConstraintPoster = new FZCBLSConstraintPoster(cblsmodel.c, cblsmodel.getIntValue)

    postInvariants(cblsmodel, poster, invariants, log)

    postConstraints(poster, softConstraints, log)



    //Do not want to search on fixed variables!
    cblsmodel.removeControlledVariables(v => v.domainSize == 1 || v.isControlledVariable)

    cblsmodel.addDefaultNeighbourhoods()



    //Create variable violation before closing the constraint system!
    cblsmodel.initiateVariableViolation()

    //cblsmodel.closeConstraintSystem() //The objective depends on the violation of the CS, so it must be first closed
    // before creating the Objective.
    //cblsmodel.initObjective() //But objective is needed in neighbourhoods
    cblsmodel.initObjectiveAndCloseConstraintSystem()

    cblsmodel.createNeighbourhoods() //So we actually create the neighbourhoods only after!


    if (cblsmodel.neighbourhoods.length == 0) {
      log(0, "No neighbourhood has been created. Aborting!")
      cblsmodel.handleSolution()
      return
    }


    log("Using " + cblsmodel.vars.length + " Search Variables in default assign neighbourhood")
    log("Using " + cblsmodel.vars.filter(
      v => v.min == 0 && v.max == 1).length + " Search Variables in default flip neighbourhood")
    cblsmodel.vars.foreach(v => log(2, "Search with " + v + " dom: " + v.min + ".." + v.max))
    log("Created all Neighborhoods")

    //Search
    val timeout = (if (opts.timeOut > 0) {
      opts.timeOut
    } else {
      20 * 60
    }) * 1000
    log("Timeout is set to " + timeout + " milliseconds");

    //val timeLimit = (timeout*0.04).toInt
    val timeLimit = (timeout*0.02).toInt
    //// val (sc: SearchControl, search: Chain) = createSearchProcedure(timeout, log, fzModel, opts, cblsmodel)

    //val bets = Array.tabulate(10)(i => createSearchProcedure(timeLimit+i*timeLimit, true, log, fzModel, opts, cblsmodel))

    val finalRun = createSearchProcedure(timeout, true, log, fzModel, opts, cblsmodel)

    cblsmodel.close()
    log("Model closed");

    cblsmodel.neighbourhoods.foreach(_.reset())

    var sc:SearchControl = finalRun._1
    var bestKnownObjective = Int.MaxValue
    if (opts.is("no-run")) {
      log("Not running the search...")
    } else {
      log("Starting Search at " + getWatchString)
      if (cblsmodel.c.violatedConstraints.length == 0 && fzModel.search.obj == Objective.SATISFY) {
        cblsmodel.handleSolution()
      } else {
/*
        //Bet and run precedure:
        val res = bets.map( (s:(SearchControl,SearchProcedure)) => {
          cblsmodel.neighbourhoods.foreach(_.reset())
          val c = s._1
          val sr = s._2
          System.err.println("% Starting new bet in bet-and-run scheme")
          sr.run()
          System.err.println("% Found objective: " + c.bestKnownObjective)
          if (c.bestKnownObjective < bestKnownObjective){
            System.err.println("% Found new best known objective: " + c.bestKnownObjective)
            bestKnownObjective = c.bestKnownObjective
            sc = c
          }
          c.bestKnownObjective
        }
        )
        System.err.println("% Result of bets: " + res.mkString(", "))
*/
      }
      //System.err.println("% Best result after bets = " + bestKnownObjective)
      //System.err.println("% Starting long run ")
      sc.restoreBestSolution()
      finalRun._2.run()
      finalRun._1.bestKnownObjective = sc.bestKnownObjective
      finalRun._1.restoreBestSolution()
      sc = finalRun._1
      log("Done at " + getWatchString)

      if (sc.bestKnownObjective == Int.MaxValue && cblsmodel.c.violatedConstraints.length > 0) {
        log("Did not find any solution.")
        log("Smallest violation: " + sc.bestPair._1)
        log(cblsmodel.c.violatedConstraints.length + " violated constraints")
      } else {
        log(
          "Best Overall Solution: " + sc.bestKnownObjective * (if (fzModel.search.obj == Objective.MAXIMIZE) -1 else 1))
      }
    }
    System.exit(0)
  }

  private def createSearchProcedure(timeout: Int, useCP: Boolean, log: Log,
                                    fzModel: FZProblem, opts: Options,
                                    cblsmodel: FZCBLSModel) = {
    val sc: SearchControl = fzModel.search.obj match {
      case Objective.SATISFY => new SearchControl(cblsmodel, 0, timeout, true);
      case Objective.MAXIMIZE => new SearchControl(cblsmodel, -fzModel.search.variable.get.max, timeout, false);
      case Objective.MINIMIZE => new SearchControl(cblsmodel, fzModel.search.variable.get.min, timeout, false);
    }

    val objDom = fzModel.search.obj match {
      case Objective.SATISFY => None
      case Objective.MAXIMIZE => Some(cblsmodel.objective.objectiveVar.domain)
      case Objective.MINIMIZE => Some(cblsmodel.objective.objectiveVar.domain)
    }
    log("Objective dom is: " + objDom.getOrElse(null))

    val search = new Chain(
      new ActionSearch(() => {
        if(opts.is("usecp")){
          cblsmodel.useCP = useCP
        }
        if(objDom.isDefined) {
          log("Objective dom is: " + objDom)
          cblsmodel.objective.violationWeight := 1
          cblsmodel.objective.objectiveWeight := 1
          cblsmodel.objective.objectiveVar.asInstanceOf[ChangingIntValue].relaxDomain(objDom.get)
          cblsmodel.objective.bound.get := (fzModel.search.obj match {
            case Objective.MAXIMIZE => cblsmodel.objective.objectiveVar.min
            case Objective.MINIMIZE => cblsmodel.objective.objectiveVar.max
          })
        }
      }),
      new ActionSearch(() => {
        val searchVariables = cblsmodel.neighbourhoods.foldLeft(Set.empty[CBLSIntVar])((acc: Set[CBLSIntVar], x: Neighbourhood) => acc ++ x.getVariables().filterNot(_.isInstanceOf[StoredCBLSIntConst])).toArray
        sc.cancelObjective()
      }),
      if (!opts.is("no-sls")) new SimpleLocalSearch(cblsmodel, sc) else new ActionSearch(() => {}),
      new NeighbourhoodSearchSAT(cblsmodel, sc),
      new ActionSearch(() => {
        val searchVariables = cblsmodel.neighbourhoods.foldLeft(Set.empty[CBLSIntVar])((acc: Set[CBLSIntVar], x: Neighbourhood) => acc ++ x.getVariables().filterNot(_.isInstanceOf[StoredCBLSIntConst])).toArray
        sc.restoreObjective()
      }),
      fzModel.search.obj match {
        case Objective.SATISFY => new ActionSearch(() => {})
        case Objective.MAXIMIZE => new NeighbourhoodSearchOPT(cblsmodel, sc)
        case Objective.MINIMIZE => new NeighbourhoodSearchOPT(cblsmodel,
                                                              sc); //new NeighbourhoodSearchOPTbySAT(cblsmodel,sc)//
      });

    log("Search created")
    (sc, search)
  }

  private def removeNeighbourhoodVariables(fzModel: FZProblem,
                                           cblsmodel: FZCBLSModel) = {
    for (definedNeighbourhood <- fzModel.neighbourhoods) {
      val toRemove = definedNeighbourhood.getControlledVariables
      cblsmodel.removeControlledVariables(v => toRemove.exists(p => p.id == v.name))
    }
  }

  def postConstraints(poster: FZCBLSConstraintPoster, softConstraints: List[Constraint], log: Log): Unit = {
    for (constraint <- softConstraints) {
      log(2, "Posting as Soft " + constraint)
      poster.construct_and_add_constraint(constraint);
    }
    log("Posted " + softConstraints.length + " Soft Constraints")
    Helper.getCstrsByName(softConstraints).map
    { case (n: String, l: List[Constraint]) => l.length + "\t" + n }.toList.sorted.foreach(s => log(" " + s))
    log(softConstraints.filter(c => c.getVariables().forall(v => !v.isDefined)).size + " are only on search variables.")
    log(softConstraints.filter(c => c.getVariables().forall(v => v.isDefined)).size + " are only on defined variables.")
  }

  def postInvariants(cblsmodel: FZCBLSModel, poster: FZCBLSConstraintPoster, invariants: List[Constraint],
                     log: Log): Unit = {
    for (invariant <- invariants) {
      log(2, "Posting as Invariant " + invariant)
      val inv = poster.construct_and_add_invariant(invariant)
      cblsmodel.cblsIntMap += invariant.definedVar.get.id -> inv;
    }

    log("Posted " + invariants.length + " Invariants")
    Helper.getCstrsByName(invariants).map
    { case (n: String, l: List[Constraint]) => l.length + "\t" + n }.toList.sorted.foreach(s => log(" " + s))
  }

  def findAndPostImplicitConstraints(cblsmodel: FZCBLSModel, maybeSoftConstraint: List[Constraint], opts: Options,
                                     log: Log): List[Constraint] = {
    val softOrImplicitConstraint = maybeSoftConstraint
    if (opts.is("no-impl-cstr")) {
      log("Did not try to find implicit constraints")
      softOrImplicitConstraint
    } else {
      val implicitPoster = new FZCBLSImplicitConstraints(cblsmodel)
      //Implicit constraints are posted in this metod call.
      val (implicitConstraints, softConstraints) = implicitPoster.findAndPostImplicit(softOrImplicitConstraint)
      //TODO: Add the implicitConstraints to some system to ensure that they are at all time respected.
      log("Found " + cblsmodel.neighbourhoodGenerator.length + " Implicit Constraints")
      Helper.getCstrsByName(implicitConstraints).map
      { case (n: String, l: List[Constraint]) => l.length + "\t" + n }.toList.sorted.foreach(s => log(" " + s))

      val hardCS = ConstraintSystem(cblsmodel.m)
      val hardPoster: FZCBLSConstraintPoster = new FZCBLSConstraintPoster(hardCS, cblsmodel.getIntValue);
      for (c <- implicitConstraints) {
        try {
          hardPoster.construct_and_add_constraint(c)
        } catch {
          case e: NoSuchConstraintException => log("Warning: Do not check that " + c + " is always respected.")
        }
      }
      hardCS.close()
      var hardConstraintError = false
      Event(hardCS.violation, Unit => {
        if (hardCS.violation.value > 0) {
          if(hardConstraintError){
            log("ERROR: Some implicit Constraint is not satisfied during search, neighbourhood restart did not help.")
            throw new Exception()
          }
          hardConstraintError = true
          log("WARNING: Some implicit Constraint is not satisfied during search.")
          cblsmodel.neighbourhoods.foreach(
            n => log(0, n.getClass().toString() + " " + n.getVariables().mkString("[", ",", "]")))
          log("Trying soft restart of neighbourhoods...")
          cblsmodel.neighbourhoods.foreach( _.reset())

        }else{
          hardConstraintError = false
          log("Soft restart resolved the issue, for now.")
        }
      });
      //Event(cs.violation, Unit => {log(cs.violation.toString);})
      //
      softConstraints
    }
  }

  def findInvariants(model: FZProblem, opts: Options, log: Log): Unit = {
    val searchVars = model.neighbourhoods.flatMap(_.getSearchVariables)
    if (opts.is("no-find-inv")) {
      log("Did not search for new invariants")
    } else {
      FZModelTransfo.findInvariants(model, log, searchVars);
      log("Found Invariants")
    }
    //If a variable has a domiain of size 1, then do not define it with an invariant.
    //This should be redundant as FZModelTransfo.findInvariants does not consider bound variables.
    for (c <- model.constraints) {
      if (c.definedVar.isDefined && c.definedVar.get.isBound) {
        c.unsetDefinedVar(c.definedVar.get)
      }
    }

    if (opts.is("no-post-inv")) {
      for (c <- model.constraints) {
        if (c.definedVar.isDefined) {
          c.unsetDefinedVar(c.definedVar.get)
        }
      }
    }
  }

  private def simplifyFlatZincModel(opts: Options, log: Log, useCP: Boolean, model: FZProblem,
                                    cpmodel: FZCPModel): Unit = {
    if (useCP) {
      FZModelTransfo.propagate(model)(log);
      log("Reduced Domains before CP")
      //println(fzModel.variables.toList.map(v => v.domainSize))
      cpmodel.createVariables()
      cpmodel.createConstraints()
      cpmodel.updateIntermediateModelDomains()
      log("Reduced Domains with CP")
      //println(fzModel.variables.toList.map(v => v.domainSize))
    }
    if (opts.is("no-simpl")) {
      log("No domain reduction")
    } else {
      //TODO: check which part of the following is still necessary after using CP for bounds reduction.
      FZModelTransfo.simplify(model)(log);
      log("Reduced Domains")
      FZModelTransfo.simplify(model)(log);
      log("Reduced Domains")
    }
    model.constraints.foreach(c => if (c.getVariables().length <= 1) {
      log("Remaining Unary Constraint " + c)
    } else if (c.getVariables().filter(v => !v.isBound).length <= 1) {
      log("De facto Unary Constraint " + c);
      //log(2,c.getVariables().map(v => v.min+".."+v.max).mkString(" , "))
    })
    model.constraints.foreach
    { case reif(c, b) => if (b.isBound) log("Fixed reified constraint: " + b.boolValue); case _ => {} }
  }

  def createLocalConstraintSystem(constraints: Seq[Constraint], cblsmodel: FZCBLSModel): ConstraintSystem = {
    val whereConstraintSystem = ConstraintSystem(cblsmodel.m)
    val whereConstraintPoster = new FZCBLSConstraintPoster(whereConstraintSystem, cblsmodel.getIntValue)
    val (invariants, softConstraints) =
      constraints.partition(c => c.definedVar.isDefined)

    for (invariant <- invariants) {
      val inv = whereConstraintPoster.construct_and_add_invariant(invariant)
      cblsmodel.cblsIntMap += invariant.definedVar.get.id -> inv;
    }
    for (constraint <- softConstraints) {
      whereConstraintPoster.construct_and_add_constraint(constraint)
    }
    whereConstraintSystem.close()
    whereConstraintSystem
  }

}

