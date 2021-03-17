package oscar.examples.cp_xcsp3

import oscar.cp._
import oscar.modeling.models.cp.CPModel
import oscar.modeling.models.operators.CPInstantiate
import oscar.modeling.models.{ModelDeclaration, UninstantiatedModel}
import oscar.xcsp3.XCSP3Parser2

/**
  * Example of parsing and solving an XCSP3 instance with OscaR CP.
  * @author Charles Thomas cftmthomas@gmail.com
  */
object SolveXCSP3Instance extends App{
  val instancePath = "data/xcsp3/inst1.xml" //Path to the xcsp instance file

  val md = new ModelDeclaration //Model declaration

  val (variables, solutionGenerator) = XCSP3Parser2.parse(md, instancePath) //Parsing the instance and getting the variables representation

  //Creating and linking CP model:
  val model: CPModel = CPInstantiate(md.getCurrentModel.asInstanceOf[UninstantiatedModel])
  md.setCurrentModel(model)

  val cpVars: Array[CPIntVar] = variables.map(model.getRepresentative(_).realCPVar) //getting CP variables

  implicit val solver: CPSolver = model.cpSolver //getting solver

  onSolution{
    println("new solution:")
    val instantiation = solutionGenerator() //Generating xcsp3 instantiation of the solution
    println(instantiation)
  }

  //Setting search:
  search{
    conflictOrderingSearch(cpVars, cpVars(_).size, cpVars(_).min) //Conflict ordering search heuristic with min dom variable selection heuristic and min val value selection heuristic
  }

  start() //Starting search
}
