package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class FileFormatTest extends TestSuite {

  def dataset():(Array[Array[Int]], Array[Array[Int]]) = {
    (Array(Array(1, 2, 4, 3, 2), Array(2, 1, 1, 4, 3, 1, 2), Array(1, 2, 4, 4, 2, 5, 3), Array(1, 3, 3, 2)),
      Array(Array(2, 5, 6, 10, 11), Array(1, 2, 9, 12, 15, 18, 24), Array(2, 4, 6, 8, 10, 12, 14), Array(1, 2, 3, 4)))
  }

  test(" test: spade format"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.txt.sp", SpadeFormat)

    val (expectedDb, expectedDbTime) = dataset()

    assert(TestHelpers.checkArray(db.getData, expectedDb))
    assert(TestHelpers.checkArray(db.getTime, expectedDbTime))

  }

  test(" test: b spade format"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.txt.b", BSpadeFormat)

    val (expectedDb, expectedDbTime) = dataset()

    assert(TestHelpers.checkArray(db.getData, expectedDb))
    assert(TestHelpers.checkArray(db.getTime, expectedDbTime))

  }


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


  test(" test: spmf format"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.txt.spmf", SpmfFormat)

    val (expectedDb, expectedDbTime) = dataset()

    assert(TestHelpers.checkArray(db.getData, expectedDb))

  }

  test(" test: spmf with names format"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.name.spmf", SpmfWithHeaderFormat)

    val (expectedDb, expectedDbTime) = dataset()

    println(db)
    assert(TestHelpers.checkArray(db.getData, expectedDb))

  }

  test(" test: spmf with time format"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.time.spmf", SpmfWithTimeFormat)

    val (expectedDb, expectedDbTime) = dataset()

    assert(TestHelpers.checkArray(db.getData, expectedDb))
    assert(TestHelpers.checkArray(db.getTime, expectedDbTime))

  }

}
