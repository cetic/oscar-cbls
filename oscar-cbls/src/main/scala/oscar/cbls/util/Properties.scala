package oscar.cbls.util

object Properties {

  private def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
  private def prePadToLength(s: String, l: Int) = (nStrings(l-s.length, " ") + s).substring(0, l)
  private def nStrings(n: Long, s: String): String = if (n <= 0L) "" else s + nStrings(n - 1L, s)

  def justifyLeft(l:List[(String,String)], sep:String = " "):List[String] = {
    val length:Int = l.map(_._1.length).max
    l.map(a => padToLength(a._1,length) + sep + a._2)
  }

  def justifyLeftArray(l:List[Array[String]], sep:String = " "):List[String] = {
    val nbCol = l.map(_.length).max
    val lengths:Array[Int] = Array.tabulate(nbCol)(i => l.map(line => if(i < line.length)line(i).length else 0).max)
    l.map(line => Array.tabulate(Math.min(nbCol,line.length))(col => padToLength(line(col),lengths(col)+2)).mkString(""))
  }

  def justifyLeftAny(l:List[List[Any]], sep:String = " "):List[String] = {
    val nbCol = l.map(_.size).max
    val strings:List[Array[String]] = l.map(l => l.map(cell => cell.toString).toArray)
    val lengths:Array[Int] = Array.tabulate(nbCol)(col => strings.map(_ (col).length).max)
    strings.map(line => Array.tabulate(nbCol)(col => padToLength(line(col),lengths(col)+2)).mkString(""))
  }

  def justifyRightArray(l:List[Array[String]], sep:String = " "):List[String] = {
    val nbCol = l.map(_.length).max
    val lengths:Array[Int] = Array.tabulate(nbCol)(i => l.map(line => if(i < line.length)line(i).length else 0).max)
    l.map(line => Array.tabulate(Math.min(nbCol,line.length))(col => prePadToLength(line(col),lengths(col))).mkString("  "))
  }
}
