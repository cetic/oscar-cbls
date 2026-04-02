package oscar.cbls.core.distributed.search

/** A class representing test behavior that can be sent to workers, for testing purposes
  * @param crashAfterMs
  * the worker will crash after this number of milliseconds and send an exception
  * @param disconnectAfterMs
  * the worker will send a [[oscar.cbls.core.distributed.protocol.WorkersDisconnected]] message and
  * disconnect after this number of milliseconds
  * @param delayStartupByMs
  * the worker will start after this number of milliseconds
  */
case class TestBehavior(
  crashAfterMs: Int = -1,
  disconnectAfterMs: Int = -1,
  delayStartupByMs: Int = -1
) {
  override def toString: String =
    s"TestBehavior(crashAfterMs:$crashAfterMs, disconnectAfterMs:$disconnectAfterMs, delayStartupByMs:$delayStartupByMs)"
}
