package oscar.cbls.core.distrib


import java.util.concurrent.atomic.AtomicInteger

class ThreadFactory(namePrefix:String = "", desiredPriority:Int = Thread.MAX_PRIORITY) extends java.util.concurrent.ThreadFactory {
  final private val threadNumber = new AtomicInteger(1)

  override def newThread(r: Runnable) = {
    val t = new Thread(null, r, namePrefix + threadNumber.getAndIncrement, 0)
    if (t.isDaemon) t.setDaemon(false)
    if (t.getPriority != desiredPriority) t.setPriority(desiredPriority)
    t
  }
}

