package oscar.cbls.util

object Demo {
  def startUpPause() :Unit = {
    println("DEMO: pause; press ENTER to resume execution in 3 seconds")
    scala.io.StdIn.readLine()
    println("DEMO: start in 3 seconds")
    Thread.sleep(1000)
    println("DEMO: start in 2 seconds")
    Thread.sleep(1000)
    println("DEMO: start in 1 second")
    Thread.sleep(1000)
  }
}
