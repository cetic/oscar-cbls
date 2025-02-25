// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.visual

import oscar.cbls.core.computation.Store
import oscar.cbls.visual.generator.ColorGenerator
import scalafx.application.{JFXApp3, Platform}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/** Companion object of OscaRDisplay */
object OscaRDisplay {

  /** The generic visualisation tool for displaying OscaR.CBLS problem resolution.
    *
    * It follows the ScalaFX way of displaying graphical content. It's composed of :
    *   - A [[OscaRPrimaryStage]] : For instance
    *     [[oscar.cbls.visual.geographic.GeographicalRoutingDisplay]].
    *   - Optional additional [[OscaRStage]] :
    *     - [[oscar.cbls.visual.additionalStages.ObjectiveFunctionDisplay]].
    *
    * Each stage is represented as an individual window exposing a scene.
    *
    * __Note__ : The initiation of the stages has to be done during the application starting
    * procedure. That's why we use function returning Stages instead of Stages.
    *
    * @param store
    *   The store containing all the variables of the problem.
    * @param primaryStage
    *   A function returning the main stage, exposing the VRS map, WLP plan...
    * @param additionalStages
    *   A list of function returning additional stages (displayed in separated window) like the
    *   objective function curve.
    */
  def apply(
    store: Store,
    primaryStage: () => OscaRPrimaryStage,
    additionalStages: List[() => OscaRStage] = List.empty
  ): OscaRDisplay = {
    val display = new OscaRDisplay(store, primaryStage, additionalStages)
    Future(display.main(Array.empty))
    display
  }
}

/** The generic visualisation tool for displaying OscaR.CBLS problem resolution.
  *
  * It follows the ScalaFX way of displaying graphical content. It's composed of :
  *   - A [[OscaRPrimaryStage]] : For instance
  *     [[oscar.cbls.visual.geographic.GeographicalRoutingDisplay]].
  *   - Optional additional [[OscaRStage]] :
  *     - [[oscar.cbls.visual.additionalStages.ObjectiveFunctionDisplay]].
  *
  * Each stage is represented as an individual window exposing a scene.
  *
  * __Note__ : The initiation of the stages has to be done during the application starting
  * procedure. That's why we use function returning Stages instead of Stages.
  *
  * @param store
  *   The store containing all the variables of the problem.
  * @param primaryStage
  *   A function returning the main stage, exposing the VRS map, WLP plan...
  * @param additionalStages
  *   A list of function returning additional stages (displayed in separated window) like the
  *   objective function curve.
  */
class OscaRDisplay(
  store: Store,
  primaryStage: () => OscaRPrimaryStage,
  additionalStages: List[() => OscaRStage]
) extends JFXApp3 {
  // Fixing the color generator seed.
  ColorGenerator.setSeed(System.nanoTime())

  private var isInit: Boolean                = false
  private var lastRedrawInMillis: Long       = 0L
  private final val REDRAWING_DELAY_MS: Long = 100L

  private var _primaryStage: OscaRPrimaryStage    = _
  private var _additionalStages: List[OscaRStage] = List.empty

  /** Redraws all graphical items that needs to be redrawn.
    *
    * @param force
    *   If true force the redrawing even if the last redrawing was not long ago.
    */
  def redraw(force: Boolean = false): Unit = {
    if (force || System.currentTimeMillis() - lastRedrawInMillis > REDRAWING_DELAY_MS) {
      while (!isInit) Thread.sleep(10) // Waiting for each stage to be initiated.
      val additionalVariablesForExtraction =
        _primaryStage.variablesForSolutionExtraction() :::
          _additionalStages.flatMap(_.variablesForSolutionExtraction())
      val solution = store.extractSolution(additionalVariablesForExtraction)

      _primaryStage.redraw(solution)
      _additionalStages.foreach(_.redraw(solution))
      lastRedrawInMillis = System.currentTimeMillis()
    }
  }

  override def start(): Unit = {
    _primaryStage = primaryStage()
    _additionalStages = additionalStages.map(_.apply())
    _primaryStage.init()
    _additionalStages.foreach(_.init())
    _additionalStages.foreach(_.show())
    stage = _primaryStage
    isInit = true
  }

  override def stopApp(): Unit = {
    Platform.runLater {
      stage.close()
      _additionalStages.foreach(_.close())
    }
  }
}
