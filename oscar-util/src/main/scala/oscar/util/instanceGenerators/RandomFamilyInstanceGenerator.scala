package oscar.util.instanceGenerators

import java.io._

import oscar.util.instanceGenerators.utils.Utils

/**
  * Created on 03/03/16.
  *
  * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
  */
object RandomFamilyInstanceGenerator {
  val randGen = oscar.util.RandomGenerator
  var i = 0

  def generateRandomT2PSInstance(nJobs: Int, nMachines: Int, nFamilies: Int, minDuration: Int, maxDuration: Int, minTT: Int, maxTT: Int, exactTT: Boolean) = {
    val nActivities = nJobs * nMachines
    val machines = Array.ofDim[Int](nActivities)
    val families = Array.ofDim[Int](nActivities)
    val machinesOnOneJob = Array.tabulate(nMachines)(i => i).toList
    for (j <- 0 until nJobs) {
      val randMachines = randGen.shuffle(machinesOnOneJob)
      for (m <- 0 until nMachines) {
        machines(j * nMachines + m) = randMachines(m)
        families(j * nMachines + m) = j % nFamilies
      }
    }
    val durations = Array.tabulate(nActivities)(i => minDuration + randGen.nextInt(maxDuration - minDuration + 1))
    val familyMatrix = if (exactTT) {
      val ttArray = Array.tabulate(nFamilies)(i => 10 * i)
      Array.tabulate(nFamilies, nFamilies)((i, j) => {
        ttArray(((nFamilies - i) + j) % nFamilies)
      })
    }
    else {
      Utils.getTriangularTTMatrix(nFamilies, minTT, maxTT, randGen)
    }

    val ttMatrix = Array.tabulate(nActivities + 1)(i => {
      if (i == 0) {
        Array.tabulate(nActivities + 1)(j => if (j == 0) 0 else randGen.nextInt(maxTT + 1))
      }
      else {
        Array.tabulate(nActivities + 1)(j => if (j == 0 || j == i) 0 else familyMatrix(families(i - 1))(families(j - 1)))
      }
    })

    (nActivities, nJobs, nMachines, machines, durations, ttMatrix)
  }

  def generateRandomJobShopInstance(nJobs: Int, nMachines: Int, nFamilies: Int, minDuration: Int, maxDuration: Int, minTT: Int, maxTT: Int, familiesOfEqualSize: Boolean = true, probaOptional: Float) = {
    val nActivities = nJobs * nMachines
    val machines = Array.ofDim[Int](nActivities)
    val machinesOnOneJob = Array.tabulate(nMachines)(i => i).toList
    for (j <- 0 until nJobs) {
      val randMachines = randGen.shuffle(machinesOnOneJob)
      for (m <- 0 until nMachines) {
        machines(j * nMachines + m) = randMachines(m)
      }
    }
    val durations = Array.tabulate(nActivities)(i => minDuration + randGen.nextInt(maxDuration - minDuration + 1))
    val familiesNotYetShuffled = Array.tabulate(nActivities)(i => if (familiesOfEqualSize) i % nFamilies else randGen.nextInt(nFamilies))
    val families = randGen.shuffle(familiesNotYetShuffled.toList).toArray
    val familyMatrix = Utils.getTriangularTTMatrix(nFamilies, minTT, maxTT, randGen)
    val isOptional = Array.fill(nActivities)(randGen.nextFloat() < probaOptional)

    val ttMatrix = Array.tabulate(nActivities + 1)(i => {
      if (i == 0) {
        Array.tabulate(nActivities + 1)(j => if (j == 0) 0 else randGen.nextInt(maxTT + 1))
      }
      else {
        Array.tabulate(nActivities + 1)(j => if (j == 0 || j == i) 0 else familyMatrix(families(i - 1))(families(j - 1)))
      }
    })

    (nActivities, nJobs, nMachines, machines, durations, ttMatrix, isOptional)
  }

  def writeT2PSInstance(outPath: String, minTime: Int, nJ: Int, nM: Int, nFamilies: Int, minDuration: Int, maxDuration: Int, minTT: Int, maxTT: Int, exactTT: Boolean = false): Unit = {
    // Getting data from the instance
    val (nActivities, nJobs, nMachines, machines, durations, ttMatrix) = generateRandomT2PSInstance(nJ, nM, nFamilies, minDuration, maxDuration, minTT, maxTT, exactTT)
    // Writing the instance in a file
    val file = new File(outPath)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("1\n")
    bw.write(s"$nJobs 9999 9999 9999 9999 9999 9999 9999 9999\n")
    for (i <- 0 to nJobs) {
      bw.write(s"$nMachines\n")
    }
    for (a <- 0 until nActivities) {
      bw.write(s"${machines(a) + 1} ${durations(a)} 0 0\n")
    }
    for (i <- 0 to nActivities) {
      for (j <- 0 to nActivities) {
        bw.write(s"${ttMatrix(i)(j)}\n")
      }
    }
    bw.close()
  }
}
