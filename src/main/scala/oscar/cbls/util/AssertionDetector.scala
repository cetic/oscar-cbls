package oscar.cbls.util

/** Handles methods related to scala assertions. */
object AssertionDetector {

  // Returns true if assertion are activated.
  def isAssertionActivated(): Boolean = {
    var activated = false
    assert({ activated = true; activated }, "")
    activated
  }

}
