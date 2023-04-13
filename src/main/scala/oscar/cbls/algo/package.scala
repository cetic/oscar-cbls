package oscar.cbls

/**
 * this package contains a set of algorithms and data structure that are specifically developed for oscar.cbls.
 */
package object algo {
  implicit def longToInt(l:Long):Int = Math.toIntExact(l)
}
