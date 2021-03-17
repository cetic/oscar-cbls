package oscar.cp.constraints.nooverlap.util

object TransitionTimesUtils {
  def transitionMatrixFromFamilyMatrix(families: Array[Int], familyTransitionMatrix: Array[Array[Int]]): Array[Array[Int]] = {
    val nTasks = families.length
    Array.tabulate[Int](nTasks,nTasks)((i:Int, j:Int) => familyTransitionMatrix(families(i))(families(j)))
  }
}