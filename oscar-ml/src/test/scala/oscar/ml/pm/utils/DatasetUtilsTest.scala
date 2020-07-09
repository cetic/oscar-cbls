package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class DatasetUtilsTest extends TestSuite {

  test( "clean Dataset"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test.txt.sp", SpadeFormat)
    assert(TestHelpers.checkArray(DatasetUtils.cleanDataset(db, 1).getData, db.getData))

    val expectedDb = Array(Array(1, 2, 3, 2), Array(2, 1, 1, 3, 1, 2), Array(1, 2, 2, 3), Array(1, 3, 3, 2))
    assert(TestHelpers.checkArray(DatasetUtils.cleanDataset(db, 4).getData, expectedDb))
  }

  test( "test support"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test.txt.sp", SpadeFormat)

    val expectedDb = Array(4, 4, 4, 3, 1)

    assert(DatasetUtils.getSDBSupport(db) sameElements expectedDb)
  }

  test( "test sdblastpost"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test.txt.sp", SpadeFormat)

    val expectedDb = Array(
      Array(1, 5, 3, 4, 5),
      Array(7, 6, 6, 4, 5, 6, 7),
      Array(1, 5, 4, 4, 5, 6, 7),
      Array(1, 3, 3, 4)
    )

    assert(TestHelpers.checkArray(DatasetUtils.getSDBLastPos(db), expectedDb))
  }

  test( "test lastpostitem"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test.txt.sp", SpadeFormat)

    val expectedDb = Array(
      Array(0, 0, 0, 0),
      Array(1, 6, 1, 1),
      Array(5, 7, 5, 4),
      Array(4, 5, 7, 3),
      Array(3, 4, 4, 0),
      Array(0, 0, 6, 0)
    )

    assert(TestHelpers.checkArray(DatasetUtils.getItemLastPosBySequence(db), expectedDb))
  }

  test( "test supports"){
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test.txt.sp", SpadeFormat)

    val expectedDb = Array(4, 4, 4, 3, 1)

    assert(DatasetUtils.getSDBSupport(db) sameElements expectedDb)
  }

  test("test nextPostGap") {
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/spm/test.txt.sp", SpadeFormat)

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
    val db = Dataset("oscar-ml/src/main/scala/oscar/ml/pm/data/fem/papertestT.txt", LongSequenceTimeFormat)

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

}
