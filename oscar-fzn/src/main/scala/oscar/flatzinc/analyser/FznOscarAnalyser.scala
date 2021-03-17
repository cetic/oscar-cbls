/*******************************************************************************
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
  ******************************************************************************/
/**
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.analyser

import oscar.flatzinc._
import oscar.flatzinc.parser.FZParser
import oscar.flatzinc.model.Helper
import oscar.flatzinc.model.BooleanVariable
import oscar.flatzinc.parser.Model
import oscar.flatzinc.transfo.FZModelTransfo

object FznOscarAnalyser extends FznOscarMain{
  checkAntlr()
  withCheck{
    val opts = options("fzn-oscar-analyser",cbls=false)
    val log = opts.log()
    log("start")
    val fname = opts.fileName
    val model = FZParser.readFlatZincModelFromFile(opts.fileName,log, true)
    log("Parsed.")
    printInfo(model,log)
    
    /*FZModelTransfo.propagateDomainBounds(model.problem)(log);
    log("reduction")
    printInfo(model,log)*/
    log("done")
  }
  
  def printInfo(model: Model, log: Log): Unit ={
    val log = new Log(1);
    log("Variables: "+model.problem.variables.size)
    log("Introduced Variables: "+model.problem.variables.filter(v => !v.anns.isEmpty && model.isIntroducedVar(v.id)).size)
    log("Defined Variables: "+model.problem.variables.filter(v => v.isDefined).size)
    log("Bound Variables: "+model.problem.variables.filter(v => v.isBound).size)
    log("Boolean Variables: "+model.problem.variables.filter(v => v.isInstanceOf[BooleanVariable]).size)
    log("Constraints: "+model.problem.constraints.size)
    log("Constraints Types:")
    Helper.getCstrsByName(model.problem.constraints).map{case(n,c) => (n,c.length)}.toList.sortBy(-_._2).foreach{case(n,c) => log(f"> $c%6s  $n%s")}
    log("Variable Degrees:")
    Helper.getVarDegreeDistribution(model.problem.variables).toList.sortBy(_._1).foreach{case(d,c) => log(f"> $d%3s $c%5s")}
    log("Constraint Degrees:")
    Helper.getConsDegreeDistribution(model.problem.constraints).toList.sortBy(_._1).foreach{case(d,c) => log(f"> $d%3s $c%5s")}
    log("Effective Constraint Degrees:")
    Helper.getRealConsDegreeDistribution(model.problem.constraints).toList.sortBy(_._1).foreach{case(d,c) => log(f"> $d%3s $c%5s")}
    log("Variable FzDomain Sizes:")
    Helper.getVarDomainDistribution(model.problem.variables).toList.sortBy(_._1).foreach{case(d,c) => log(f"> $d%3s $c%5s")}
    if(!model.problem.variables.filter(v => v.cstrs.isEmpty).isEmpty){
      log("Unconstrained variables:")
      model.problem.variables.filter(v => v.cstrs.isEmpty).foreach(v => log("> "+v))
    }
    if(!model.problem.constraints.filter(c => c.variables.filter(v => !v.isBound).isEmpty).isEmpty){
      log("Empty Constraints:")
      model.problem.constraints.filter(c => c.variables.filter(v => !v.isBound).isEmpty).foreach(c => log("> "+c))
    }
  }
}