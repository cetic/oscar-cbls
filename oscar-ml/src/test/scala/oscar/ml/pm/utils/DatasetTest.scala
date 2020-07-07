package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class DatasetTest extends TestSuite {

  test("check dataset parameters") {

    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/mushroom.txt")

    db.nbTrans should be(8124)
    db.nbItem should be (120)
  }
}
