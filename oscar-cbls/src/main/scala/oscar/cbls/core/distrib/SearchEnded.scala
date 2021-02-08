package oscar.cbls.core.distrib

import akka.actor.typed.ActorRef

abstract sealed class SearchEnded(val searchID: Long)

final case class SearchCompleted(override val searchID: Long, searchResult: IndependentSearchResult) extends SearchEnded(searchID)

final case class SearchAborted(override val searchID: Long) extends SearchEnded(searchID)

final case class SearchCrashed(override val searchID: Long, neighborhood: RemoteNeighborhoodIdentification, exception: Throwable, worker: ActorRef[MessageToWorker]) extends SearchEnded(searchID)
