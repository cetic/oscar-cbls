package oscar.ml.pm.examples

import oscar.cp.CPModel
import oscar.ml.pm.examples.fim.{coverSizeRunner, globalFimRunner, pureFimRunner}

/**
 * Run everything - starting point
 *
 * @author johnaoga@gmail.com
 *
 */

object Run extends CPModel with App {

  val choix = "" // Change here

  val t1 = System.currentTimeMillis()

  choix match {
    case "C" => coverSizeRunner.main(args)
    case "G" => globalFimRunner.main(args)
    case _ => pureFimRunner.main(args)
  }

  val t2 = System.currentTimeMillis()

  println("Total time: " + (t2 - t1))
}
