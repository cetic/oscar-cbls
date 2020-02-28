package oscar.xcsp3

import java.io.ByteArrayInputStream

import org.xcsp.parser.callbacks.SolutionChecker
import scala.collection.JavaConverters._

/**
  * A "library-friendly" version of SolutionChecker
  * @param filename the filename of the XCSP3 instance on which the solution will be checked
  * @param solution the solution, as an XCSP3-valid XML string
  */
class CheckerLib(filename: String, solution: String) {
  private val checker = new SolutionChecker(false, filename, new ByteArrayInputStream(solution.getBytes))
  def getViolatedCtrs: List[String] = checker.violatedCtrs.asScala.toList
  def getInvalidObjs: List[String] = checker.invalidObjs.asScala.toList
  def valid: Boolean = checker.violatedCtrs.isEmpty && checker.invalidObjs.isEmpty
}
