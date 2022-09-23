package oscar.cbls.core.distrib

import akka.actor.typed.ActorRef

sealed trait SearchEnded {
  val uniqueSearchID: Long
}

final case class SearchCompleted[ResultType](uniqueSearchID: Long,
                                             searchResult: ResultType,
                                             durationMS: Int)
  extends SearchEnded

final case class SearchAborted(uniqueSearchID: Long)
  extends SearchEnded

final case class SearchCrashed(uniqueSearchID: Long,
                               searchTask: Option[RemoteTaskIdentification],
                               exception: Throwable,
                               worker: ActorRef[MessageToWorker])
  extends SearchEnded
