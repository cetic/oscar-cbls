package oscar.cbls.core.distrib

import akka.actor.typed.ActorRef

abstract sealed class SearchEnded(uniqueSearchID: Long)

final case class SearchCompleted[ResultType](uniqueSearchID: Long, searchResult:ResultType, durationMS:Int)
  extends SearchEnded(uniqueSearchID)

final case class SearchAborted(uniqueSearchID: Long)
  extends SearchEnded(uniqueSearchID)

final case class SearchCrashed(uniqueSearchID: Long, searchTask: Option[RemoteTaskIdentification],
                               exception: Throwable, worker: ActorRef[MessageToWorker])
  extends SearchEnded(uniqueSearchID)
