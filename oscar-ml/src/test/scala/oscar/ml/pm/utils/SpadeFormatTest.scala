package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class SpadeFormatTest extends TestSuite {

  def dataset():(Array[Array[Int]], Array[Array[Int]]) = {
    (Array(Array(1, 2, 4, 3, 2), Array(2, 1, 1, 4, 3, 1, 2), Array(1, 2, 4, 4, 2, 5, 3), Array(1, 3, 3, 2)),
      Array(Array(2, 5, 6, 10, 11), Array(1, 2, 9, 12, 15, 18, 24), Array(2, 4, 6, 8, 10, 12, 14), Array(1, 2, 3, 4)))
  }

  test(" test: spade format"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test.txt.sp", SpadeFormat)

    val (expectedDb, expectedDbTime) = dataset()

    assert(TestHelpers.checkArray(db.getData, expectedDb))
    assert(TestHelpers.checkArray(db.getTime, expectedDbTime))

  }

}
