package oscar.perf

import org.scalatest.{Assertions, FunSuite, Matchers}

abstract class PerfTest(name: String) extends FunSuite with Matchers with Assertions {
  test(name) {
    perfTest()
  }

  def perfTest(): Unit
}

class AppPerfTest(objs: Object{def main(args: Array[String]):Unit}*) extends FunSuite with Matchers with Assertions {
  for(obj <- objs) {
    test(obj.getClass.getName) {
      obj.main(Array())
    }
  }
}