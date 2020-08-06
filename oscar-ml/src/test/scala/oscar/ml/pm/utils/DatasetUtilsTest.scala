package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class DatasetUtilsTest extends TestSuite {

  test( "clean Dataset"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.txt.sp", SpadeFormat)
    assert(TestHelpers.checkArray(DatasetUtils.cleanDataset(db, 1).getData, db.getData))

    val expectedDb = Array(Array(1, 2, 3, 2), Array(2, 1, 1, 3, 1, 2), Array(1, 2, 2, 3), Array(1, 3, 3, 2))
    assert(TestHelpers.checkArray(DatasetUtils.cleanDataset(db, 4).getData, expectedDb))
  }

  test( "test support"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.txt.sp", SpadeFormat)

    val expectedDb = Array(4, 4, 4, 3, 1)

    assert(DatasetUtils.getSDBSupport(db) sameElements expectedDb)
  }

  test( "test sdblastpost"){
    var db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test2.data")
    db = DatasetUtils.cleanDataset(db, 2)

    val expectedDb = Array(
      Array(5, 4, 1),
      Array(4, 3, 2),
      Array(2, 1),
      Array(2, 1)
    )

    //TestHelpers.printMat(DatasetUtils.getSDBLastPos(db, DatasetUtils.getItemLastPosBySequence(db)))

    assert(TestHelpers.checkArray(DatasetUtils.getSDBLastPos(db, DatasetUtils.getItemLastPosBySequence(db)), expectedDb))
  }

  test( "test lastpostitem"){
    var db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test2.data")
    db = DatasetUtils.cleanDataset(db, 2)


    val expectedDb = Array(
      Array(0, 1, 4, 5, 0),
      Array(0, 2, 3, 4, 0),
      Array(0, 1, 2, 0, 0),
      Array(0, 0, 1, 2, 0)
    )

    assert(TestHelpers.checkArray(DatasetUtils.getItemLastPosBySequence(db), expectedDb))
  }

  test( "test supports"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.txt.sp", SpadeFormat)

    val expectedDb = Array(4, 4, 4, 3, 1)

    assert(DatasetUtils.getSDBSupport(db) sameElements expectedDb)
  }

  test("test nextPostGap") {
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test/input/test.txt.sp", SpadeFormat)

    val expected = Array(
      Array(1, 3, 3, 6, 6),
      Array(2, 2, 3, 4, 5, 6, 8),
      Array(2, 3, 4, 5, 6, 8, 8),
      Array(3, 5, 5, 5)
    )

    val actual = DatasetUtils.getSDBNextPosGap(db, 3)

    assert(TestHelpers.checkArray(actual, expected))
  }

  test("test LS nextPostGap") {
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/input/test.time.txt", LongSequenceTimeFormat)

    val expected = Array(
      Array(-1, 5, 4, 3),
      Array(-1, 5, 4, 3),
      Array(-1, 5, 6, 3),
      Array(-1, 5, 6, 3),
      Array(-1, 5, 6, -1),
      Array(-1, 5, 6, -1),
      Array(-1, -1, 6, -1)
    )

    val actual = DatasetUtils.getLSNextPosGap(db, 10)

    assert(TestHelpers.checkArray(actual, expected))
  }

  test( "test global data structures computation -  real dataset"){
    //var db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/fifa.dat.sdb")
    //var db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/Kosarak.txt.sdb")
    var db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test1.data")

    db = DatasetUtils.cleanDataset(db, 2) //40%

    var (a, b, c , d) = DatasetUtils.precomputedDatastructures(db)
    var (w, x, y, z) = TestHelpers.testPrecomputedDatastructures(db)
    a = 0 +: a

    assert(a sameElements w)
    assert(TestHelpers.checkArray(b, x))
    assert(TestHelpers.checkArray(c, y))
    assert(TestHelpers.checkArray(d, z))
  }


  test( "test LS support"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/input/test.fasta", ProteinLongSequence)

    val expectedDb = Array(3, 2, 1)

    assert(DatasetUtils.getLSSupport(db) sameElements expectedDb)
  }

  test( "test LS frequent items"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/fem/test/input/test.fasta", ProteinLongSequence)

    val expectedDb = Array(1, 2)

    assert(DatasetUtils.getLSFrequentItems(db, 2) sameElements expectedDb)
  }

}
