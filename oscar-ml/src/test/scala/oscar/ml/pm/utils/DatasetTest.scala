package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class DatasetTest extends TestSuite {

  test("check dataset parameters") {

    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/fim/mushroom.txt")

    db.nbTrans should be(8124)
    db.nbItem should be (120)
  }

  test("IntoVertical: convert dataset into vertical dataset") {

    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/fim/test.txt")
    val vtdb = db.intoVertical()

    val output = Array(Set(), Set(0, 2, 3), Set(0, 1, 2, 3, 4), Set(1, 3, 4), Set(0, 1, 2, 3))

    var i = 0
    while (i < vtdb.length) {
      assert (vtdb(i) == output(i) )
      i += 1
    }
  }


}
