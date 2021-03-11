package oscar.cp.examples

import oscar.cp._

/** @author Pierre Schaus */
object LargeScaleGreedyCumulative extends CPModel with App {

  val nTasks = 10000
  val rand = new scala.util.Random(0)

  val durations = Array.fill(nTasks)(rand.nextInt(200) + 5)
  val height = Array.fill(nTasks)(rand.nextInt(40) + 1)
  val horizon = 1000000000
  val capa = 100

  val durationsVar = Array.tabulate(nTasks)(t => CPIntVar(durations(t)))
  val startsVar = Array.tabulate(nTasks)(t => CPIntVar(0, horizon))
  val endsVar = Array.tabulate(nTasks)(t => startsVar(t) + durationsVar(t))
  val demandsVar = Array.tabulate(nTasks)(t => CPIntVar(height(t)))
  val capacity = CPIntVar(capa)

  add(maxCumulativeResource(startsVar, durationsVar, endsVar, demandsVar, capacity), Weak)

  val t0 = System.currentTimeMillis

  for (t <- 0 until nTasks) {
    if (t % 1000 == 0) println(t)
    solver.doAndPropagate {
      val variable = startsVar(t)
      variable.assign(variable.min)
    }
  }

  val time = System.currentTimeMillis - t0

  println(time)
  println("max=" + endsVar.map(_.value).max)
}
