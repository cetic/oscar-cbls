package oscar.cp.test


import oscar.algo.branchings.BinaryStaticOrderBranching
import oscar.cp.core.Constraint
import oscar.cp.scheduling.constraints._
import oscar.cp._
import oscar.cp.testUtils._
import oscar.cp.core.CPPropagStrength


abstract class TestCumulativeConstraint(val cumulativeName: String, val nTests: Int = 100, val minDuration: Int = 0, val k: Int = 5) extends TestSuite {

  type Sol = List[Int]

  case class SchedulingInstance(val durations: Array[Int], val demands: Array[Int], val resources: Array[Int], val capacity: Array[Int], val horizon: Int) {
    override def toString: String = {
      val dur = "durations: " + durations.mkString(", ")
      val dem = "demands: " + demands.mkString(", ")
      val res = "resources: " + resources.mkString(", ")
      val cap = "capacity: " + capacity.mkString(", ")
      val hor = "horizon: " + horizon
      dur + "\n" + dem + "\n" + res + "\n" + cap + "\n" + hor
    }
  }

  class CPSched(instance: SchedulingInstance) extends CPSolver {
    implicit val solver = this
    silent = true
    val nTasks = instance.demands.size
    val Tasks = 0 until nTasks
    val nResources = instance.capacity.size
    val Resources = 0 until nResources
    val durations = Array.tabulate(nTasks)(t => CPIntVar(instance.durations(t)))
    val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to instance.horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPIntVar(durations(t).min to instance.horizon))
    val demands = Array.tabulate(nTasks)(t => CPIntVar(instance.demands(t)))
    val resources = Array.tabulate(nTasks)(t => CPIntVar(instance.resources(t)))
    Tasks.foreach(t => post(ends(t) === starts(t) + durations(t)))
  }

  def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Array[Constraint]  // returning array is useful when testing non-checking constraints, such as edge-finding, to couple with cumulativedecomp  

  def generateRandomSchedulingProblem(nTasks: Int): SchedulingInstance = {
    val rand = new scala.util.Random()
    val durations = Array.fill(nTasks)(rand.nextInt(4) + minDuration)
    val demands = Array.fill(nTasks)(rand.nextInt(4))
    val capacity = Array(rand.nextInt(4) + 1)
    val resources = Array.fill(nTasks)(0)
    val horizon = rand.nextInt(durations.sum + 1 - durations.max) + durations.max
    SchedulingInstance(durations, demands, resources, capacity, horizon)
  }

  def solveAll(cp: CPSched, capacity: Array[Int], decomp: Boolean, cons: CPPropagStrength): Set[Sol] = {
    cp.deactivateNoSolExceptions()
    if (!decomp) cp.Resources.foreach(r => cp.add(cumulative(cp.starts, cp.durations, cp.ends, cp.demands, cp.resources, CPIntVar(capacity(r))(cp), r),cons))
    else cp.Resources.foreach(r => cp.add(new CumulativeDecomp(cp.starts, cp.durations, cp.ends, cp.demands, cp.resources, CPIntVar(capacity(r))(cp), r)))
    var sols: List[Sol] = List()
    
    cp.add(cp.starts(0)+2 >= cp.starts(4))
    cp.add(cp.starts(4) >= cp.ends(3)-1)
    

    cp.search {
      val b1 = binaryStaticIdx(cp.starts,i => cp.starts(i).randomValue)
      val b2 = binaryStaticIdx(cp.durations,i => cp.durations(i).randomValue)
      val b3 = binaryStatic(cp.resources)
      b1 ++ b2 ++ b3
    }

    cp.onSolution {
      val sol = cp.starts.map(_.value).toList
      sols = sol :: sols
    }

    cp.start()

    sols.toSet
  }

  def compare(sols1: Set[Sol], sols2: Set[Sol]): Boolean = {
    if (sols1.size != sols2.size) false
    else sols1.forall(s => sols2 contains s)
  }

  test("test solveAll " + cumulativeName) {
    for (i <- 1 to nTests) {
      //print("test " + cumulativeName + " instance " + i + ": ")
      val instance = generateRandomSchedulingProblem(k)
      val cpDecomp = new CPSched(instance)
      val allSolsDecomp = solveAll(cpDecomp, instance.capacity, true, CPPropagStrength.Weak)
      //println(allSolsDecomp.size)

      for (cons <- Array(CPPropagStrength.Weak)) {
        val cpCumul = new CPSched(instance)
        val allSolsCumul = solveAll(cpCumul, instance.capacity, false, cons)

        val ok = compare(allSolsDecomp, allSolsCumul)
        if (!ok) {
          print("test " + cumulativeName + " instance " + i + ": ")
          println("failed ! with consistency " + cons)
          println("expected number of solutions: " + allSolsDecomp.size)
          println("number of solutions: " + allSolsCumul.size)
          println("INSTANCE")
          println(instance)

          false
        }
        ok should be(true)
      }
    }
  }
}


class TestCumulativeDefault extends TestCumulativeConstraint("maxCumulative") {
  override def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Array[Constraint] = {
    Array(maxCumulativeResource(starts, durations, ends, demands, resources, capacity, id))
  }
}

class TestTTDR extends TestCumulativeConstraint("TTDR") {
  override def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Array[Constraint] = {
    Array(
        new CumulativeDecomp(starts, durations, ends, demands, resources, capacity, id),
        TimeTableDisjunctiveReasoning(starts, durations, ends, demands, resources, capacity, id)
    )
  }
}


class TestTT extends TestCumulativeConstraint("TT") {
  override def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Array[Constraint] = {
    Array(
        TTPerTask(starts, durations, ends, demands, resources, capacity, id)
    )
  }
}

class TestTTEF extends TestCumulativeConstraint("TTEF") {
  override def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Array[Constraint] = {
    Array(
        new CumulativeDecomp(starts, durations, ends, demands, resources, capacity, id),
        TimeTableEdgeFinding(starts, durations, ends, demands, resources, capacity, id)
    )
  }
}
