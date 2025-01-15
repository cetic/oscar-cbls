// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.test.invBench

import org.scalacheck.Test
import org.scalacheck.util.Pretty.{format, pretty, Params}

object FailureReporter {
  def apply(): FailureReporter = new FailureReporter()
}

class FailureReporter extends Test.TestCallback {
  private val prettyPrms = Params(1)

  private var _failedReports: List[String] = List.empty

  def failedReports: List[String] = _failedReports

  override def onTestResult(name: String, result: Test.Result): Unit = {
    val s = (if (result.passed) "+ " else "! ") + name + (if (name != "") ": ") + pretty(
      result,
      prettyPrms
    )

    val report = format(s, "", "", 120)
    if (result.passed) println(report)
    // If we throw an exception here, tests with additional seeds will be ignored
    // So we saved the failure report for later.
    else _failedReports = report :: _failedReports
  }
}
