package oscar.cbls.test.algo

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.magicArray.ImmutableArray

import scala.util.Random

class ImmutableArrayTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("Updating random elements keeps expected array"){
    forAll((referenceArray: Array[Int]) => {
      whenever(referenceArray.nonEmpty){
        var immutableArray = ImmutableArray.createAndImportBaseValues(referenceArray)
        val n = referenceArray.length

        for(i <- 1 to 100){
          val modifiedId = Random.nextInt(n)
          val newValue = Random.nextInt(n * (modifiedId+1))

          if(modifiedId > 0){
            referenceArray(modifiedId) = newValue
            immutableArray = immutableArray.update(modifiedId,newValue,Random.nextBoolean())
          }
        }

        for(id <- 0 until n){
          referenceArray(id) should be (immutableArray(id))
        }
      }
    })
  }

  test("iterator yields expected array"){
    forAll((referenceArray: Array[Int]) => {

      var immutableArray = ImmutableArray.createAndImportBaseValues(referenceArray)
      val n = referenceArray.length

      immutableArray.iterator.toList should be (referenceArray)
    })
  }
}
