package oscar.cp.examples.scheduling

import oscar.cp._
import scala.util.Random
import oscar.visual._
import oscar.cp.scheduling.visual._
import oscar.cp.scheduling.constraints._


/**
 * @author Steven Gay steven.gay@uclouvain.be
 */
object AlternativeUnary extends CPModel with App {

  val rand = new Random(0)
  val nTasks = 25
  val durationsData = Array.fill(nTasks)(rand.nextInt(10))
  val horizon = durationsData.sum

  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val resources = Array.fill(nTasks)(CPIntVar(0, 1))
  val one = CPIntVar(1)
  val demands = Array.fill(nTasks)(one)

  val makespan = maximum(ends)
  val nResources = 2
  
  // Resources
  for (r <- 0 until nResources) {
    add(unaryResource(starts, durations, ends, resources, r))
  }
  
  add(maxCumulativeResource(starts, durations, ends, demands, CPIntVar(2)))

  minimize(makespan) search {
//    splitLastConflict(resources ++ starts)
    val schVars = starts ++ resources
    splitLastConflict(schVars, schVars(_).size)
  }

  // Visualization  
  // -------------

  val frame = new VisualFrame("Cumulative JobShop Problem", nResources + 1, 1)
  val colors = VisualUtil.getRandomColors(nTasks, true)
  val gantt = new VisualGanttChart(starts, durations, ends, i => i, colors = i => colors(i))

  val profiles = Array.tabulate(nResources)(r => new VisualProfile(starts, durations, ends, demands, resources, 1, r, color = colors(r)))
  
  onSolution { 
    gantt.update(20, 20) 
    profiles.foreach(_.update(20, 20))
    
    println("Makespan of " + makespan.value)
    for (t <- 0 until nTasks) {
      print("Task " + t)
      print(" of duration " + durations(t).value)
      print(" is executed on resource " + resources(t).value)
      println(" at time " + starts(t).value)
    }
    println()
  }
  frame.createFrame("Gantt chart").add(gantt)
  for (r <- 0 until nResources) frame.createFrame("profile resource "+r).add(profiles(r))
  frame.pack

  println(start())
}
