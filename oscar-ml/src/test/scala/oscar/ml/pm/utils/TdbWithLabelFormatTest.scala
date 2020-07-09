package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class TdbWithLabelFormatTest extends TestSuite {
  test("split: check if the dataset is split properly according to the class labels") {

    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/fim/withlabels/test.txt", TdbWithLabelFormat)

    val (dbP, dbN) = db.splitDatasetByTwo()

    val outputP = Array(Array(1, 2, 4), Array(1, 2, 4), Array(1, 2, 3, 4))
    val outputN = Array(Array(2, 3, 4), Array(2, 3))

    assert(TestHelpers.checkArray(dbP.getData, outputP))
    assert(TestHelpers.checkArray(dbN.getData, outputN))
  }


  test("split: check if the dataset is split properly according to the dataset's properties") {

    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/fim/withlabels/anneal.txt", TdbWithLabelFormat)

    val (dbP, dbN) = db.splitDatasetByTwo()

    db.nbTrans should be (812)
    dbP.nbTrans should be (625)
    dbN.nbTrans should be (187)

    assert(db.nbItem == dbP.nbItem)
    assert(db.nbItem == dbN.nbItem)


    val (vTdbP, vTdbN) = (dbP.intoVertical(), dbN.intoVertical())
    val inputs = Array(vTdbP.indices.filter(i => vTdbP(i).isEmpty).toArray[Int], vTdbN.indices.filter(i => vTdbN(i).isEmpty).toArray[Int])
    val outputs = Array(Array(1, 2, 3, 6, 35, 47, 49), Array(1, 2, 3, 6, 7, 10, 17, 19, 21, 23))

    assert(TestHelpers.checkArray(inputs, outputs))
  }

}
