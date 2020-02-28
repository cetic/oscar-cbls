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

package oscar.modeling.solvers

import org.rogach.scallop.{ScallopConf, Subcommand}
import oscar.modeling.misc.ScallopConfProxy
import oscar.modeling.models.{ModelDeclaration, ModelDeclarationProxy}


/**
  * The base app of OscaR-Modeling. Can be linked to any type of solvers with modules, mixed-in using traits.
  */
class SolverApp[RetVal] extends SolveHolder[RetVal] with App with SolverAppModulable with ModelDeclarationProxy {
  val app = this
  val modelDeclaration: ModelDeclaration = new ModelDeclaration()
  implicit lazy val md = modelDeclaration

  // Custom configuration per app
  // Simply override config:
  // override lazy val config = new AppConfig { val myint = opt[Int]("myint","m") }
  class AppConfig extends ScallopConfProxy
  lazy val config = new AppConfig

  // Modules and config for them
  private val modules = getModules.map(x => x.subcommand -> x).toMap
  private val completeConfig = new ScallopConf(args) {
    modules.foreach(x => {
      config.applyTo(x._1)
      this.addSubcommand(x._1)
    })
  }
  completeConfig.verify()
  if(completeConfig.subcommand.isEmpty) {
    completeConfig.printHelp()
    System.exit(1)
  }

  // Module configuration
  private val moduleConfig = completeConfig.subcommand.get
  config.setProxyTo(moduleConfig.asInstanceOf[Subcommand])
  private val solverAppModule = modules(completeConfig.subcommand.get.asInstanceOf[Subcommand])

  solverAppModule.onSelect()

  def solve(): List[RetVal] = solverAppModule.solve()

  override protected val solveRedirectTo: Any = modelDeclaration
}



