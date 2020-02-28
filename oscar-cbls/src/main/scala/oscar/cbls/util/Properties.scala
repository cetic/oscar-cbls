package oscar.cbls.util

object Properties {

  private def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
  private def prePadToLength(s: String, l: Int) = (nStrings(l-s.length, " ") + s).substring(0, l)
  private def nStrings(n: Long, s: String): String = if (n <= 0L) "" else s + nStrings(n - 1L, s)

  def justifyLeft(l:List[(String,String)], sep:String = " "):List[String] = {
    val length:Int = l.map(_._1.size).max
    l.map(a => padToLength(a._1,length) + sep + a._2)
  }

  def justifyLeftArray(l:List[Array[String]], sep:String = " "):List[String] = {
    val nbCol = l.head.length
    val lengths:Array[Int] = Array.tabulate(nbCol)(i => l.map(line => line(i).size).max)
    l.map(line => Array.tabulate(nbCol)(col => padToLength(line(col),lengths(col)+2)).mkString(""))
  }

  def justifyRightArray(l:List[Array[String]], sep:String = " "):List[String] = {
    val nbCol = l.head.length
    val lengths:Array[Int] = Array.tabulate(nbCol)(i => l.map(line => line(i).size).max)
    l.map(line => Array.tabulate(nbCol)(col => prePadToLength(line(col),lengths(col))).mkString("  "))
  }
}
