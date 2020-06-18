package oscar.cbls.test



/*

import oscar.cbls.core.computation.{CBLSIntVar, IntValue}
import oscar.cbls.invariants.lib.logic.BinPackingLoad
import oscar.cbls.lib.invariant.numeric.Sum2
import oscar.cbls.modeling.CBLSModel
import oscar.cbls.util.StopWatch

/**
  * Created by gustavbjordal on 27/05/16.
  */
object MultiKnapsackTest  extends CBLSModel with StopWatch {

    def main(args: Array[String]) {


      // model
      val binArray = Array(1,1,2,2,3,3)
      val sizeArray = Array(1,2,1,2,1,2)
      val binSizes = Array(3,3,3)

      val inBinVar:Array[IntValue] = binArray.map(CBLSIntVar(_,1 to 3, "item"))
      val sizeVar:Array[IntValue] = sizeArray.map(CBLSIntVar(_,1 to 5, "size"))
      val binSizeVar:Array[CBLSIntVar] = binSizes.map(CBLSIntVar(_,1 to 20, "bin_size"))



      val bin_pack = BinPackingLoad(inBinVar.map(Sum2(_,-1)),sizeArray)
      bin_pack.Defines(binSizeVar)

      for(v <- inBinVar)
        println(c.violation(v))
      for(b <- binSizeVar)
        println(c.violation(b))

      c.close()
      // closing model
      s.close()

      def printViolation() = {
        for(b <- binSizeVar)
          println(b)
        println("---")
      }
      printViolation()
      inBinVar(0).asInstanceOf[CBLSIntVar].setValue(2)
      printViolation()
      inBinVar(4).asInstanceOf[CBLSIntVar].setValue(2)
      printViolation()
      inBinVar(0).asInstanceOf[CBLSIntVar].setValue(1)
      printViolation()
    }
  }
*/