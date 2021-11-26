package oscar.cbls.core.distrib

import akka.actor.typed.ActorRef

abstract sealed class SearchEnded[+ResultType](val uniqueSearchID: Long)

final case class SearchCompleted[ResultType](override val uniqueSearchID: Long, searchResult:ResultType, durationMS:Int)
  extends SearchEnded[ResultType](uniqueSearchID)

final case class SearchAborted(override val uniqueSearchID: Long)
  extends SearchEnded[Null](uniqueSearchID)

final case class SearchCrashed(override val uniqueSearchID: Long, searchTask: Option[RemoteTaskIdentification],
                               exception: Throwable, worker: ActorRef[MessageToWorker])
  extends SearchEnded[Null](uniqueSearchID)
