package oscar.cbls

package object test {
  implicit def longToInt(l:Long):Int = Math.toIntExact(l)
  implicit def intToLong(i:Int):Long = i
}
