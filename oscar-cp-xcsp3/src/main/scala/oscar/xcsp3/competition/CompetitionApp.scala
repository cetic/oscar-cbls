package oscar.xcsp3.competition

import scala.util.Random

class CompetitionConf(args: Seq[String]){
  type ArgMap = Map[Symbol, Any]
  val argMap: ArgMap = parseArgs(Map(), args.toList)

  def randomseed(): Long = argMap.getOrElse('randomseed, Random.nextInt(Int.MaxValue).toLong).asInstanceOf[Long] //A random seed
  def timelimit(): Int = argMap.getOrElse('timelimit, 240).asInstanceOf[Int] //The time available in seconds
  def memlimit(): Int = argMap.getOrElse('memlimit, 1000).asInstanceOf[Int] //The memory available in mb
  def nbcore(): Int = argMap.getOrElse('nbcore, 1).asInstanceOf[Int] //The number of cores available
  def tmpdir(): String = argMap.getOrElse('tmpdir, "tmpdir").asInstanceOf[String] //A temporary directory to write files
  def dir(): String = argMap.getOrElse('dir, "dir").asInstanceOf[String] //The directory containing the program
  def benchname(): String = {
    val path = argMap.getOrElse('benchname, "").asInstanceOf[String]
    if(path.isEmpty) throw new Exception("Instance path not provided!")
    path
  } //The path to the instance file

  def parseArgs(map : ArgMap, list: List[String]) : ArgMap = {
    list match {
      case Nil => map

      case "--randomseed" :: value :: tail =>
        parseArgs(map ++ Map('randomseed -> value.toLong), tail)

      case "--timelimit" :: value :: tail =>
        parseArgs(map ++ Map('timelimit -> value.toInt), tail)

      case "--memlimit" :: value :: tail =>
        parseArgs(map ++ Map('memlimit -> value.toInt), tail)

      case "--nbcore" :: value :: tail =>
        parseArgs(map ++ Map('nbcore -> value.toInt), tail)

      case "--tmpdir" :: value :: tail =>
        parseArgs(map ++ Map('tmpdir -> value), tail)

      case "--dir" :: value :: tail =>
        parseArgs(map ++ Map('dir -> value), tail)

      case benchname :: tail =>
        parseArgs(map ++ Map('benchname -> benchname), tail)
    }
  }
}

abstract class CompetitionApp extends App{
  final val tstart = System.nanoTime()
  final val version = "2017-08-22"

  //Setting up shutdown hook:
  Runtime.getRuntime.addShutdownHook(new Thread{
    override def run() {
      if(!statusPrinted) printStatus()
    }
  })

  val conf = new CompetitionConf(args)

  printComment("version: " + version)
  printComment("seed: " + conf.randomseed())
  printComment("timeout: " + conf.timelimit())
  printComment("memlimit: " + conf.memlimit())
  printComment("nbcore: " + conf.nbcore())

  Random.setSeed(conf.randomseed())
  var statusPrinted = false
  var status = "UNKNOWN"
  var currentSol = ""

  try {
    runSolver(conf)
  }catch{
    case e: Exception =>
      printDiagnostic("EXCEPTION", e.getMessage)
      printComment(e.getStackTrace.mkString("\n"))
      if(!statusPrinted) printStatus()
      Console.flush()
  }

  /**
    * TODO: Parse Instance, launch search and print results
    */
  def runSolver(conf: CompetitionConf): Unit

  // Use the following functions to print your outputs. Only the best solution should be printed.

  //Each time a new best solution is found, this method should be called:
  def updateSol(sol: String, obj: Int, cop: Boolean): Unit = {
    currentSol = sol
    if(status == "UNKNOWN") status = "SATISFIABLE"
    if(cop){
//      println(tElapsed + " o " + obj)
      println("o " + obj)
      Console.flush()
    }
  }

  //Use this only for the last solution:
  //Sol should be a valid instantiation (see rules)
  def printSolution(): Unit = {
    if(currentSol.nonEmpty) {
//      val solutionChecker = new SolutionChecker(true, conf.benchname(), new ByteArrayInputStream(("s " + status + "\nv " + currentSol.split("\\r?\\n").mkString("\nv ")).getBytes))
//      if(solutionChecker.violatedCtrs.isEmpty && solutionChecker.invalidObjs.isEmpty){
//        println(tElapsed + " s " + status)
        println("s " + status)
//        println(tElapsed + " v " + currentSol.split("\\r?\\n").mkString("\n" + tElapsed + " v "))
        println("v " + currentSol.split("\\r?\\n").mkString("\nv "))
//      }
//      else{
//        printDiagnostic("SOL_NOT_VALID")
//        printComment(currentSol)
//        println(tElapsed + " s " + "UNKNOWN")
////        println("s " + "UNKNOWN")
//      }
    }
  }

  /**
    * Allowed status:
    * OPTIMUM FOUND: the optimum has been found and proven (already printed in printSolution)
    * SATISFIABLE: a solution been found but cannot be proven optimal (already printed in printSolution)
    * UNSATISFIABLE: the instance is unsatisfiable (i.e. it is proven that no solution exists)
    * UNSUPPORTED: unsupported constraint
    * UNKNOWN: other (no solution has been found or there was a problem, use printDiagnostic to precise information)
    */
  def printStatus(): Unit = {
    if(status == "OPTIMUM FOUND" || status == "SATISFIABLE") printSolution()
    else
//      println(tElapsed + " s " + status)
      println("s " + status)
    statusPrinted = true
  }

  //For any comment:
  def printComment(com: String): Unit = {
//    println(tElapsed + " c " + com.split("\\r?\\n").mkString("\n" + tElapsed + " c "))
    println("c " + com.split("\\r?\\n").mkString("\nc "))
  }

  //For diagnostic information, name should be a keyword and value no more than one line.
  def printDiagnostic(name: String, value: String): Unit = {
//    println(tElapsed + " d " + name + " " + value)
    println("d " + name + " " + value)
  }

  //For diagnostic information, name should be a keyword.
  def printDiagnostic(name: String): Unit = {
//    println(tElapsed + " d " + name)
    println("d " + name)
  }

//  def tElapsed: Long = (System.nanoTime() - tstart)/1000000
}
