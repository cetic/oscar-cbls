package oscar

/**
 * Created by dervalguillaume on 2/12/16.
 */
package object algo {
  var debug = false
  private[algo] type Seq[+A] = scala.collection.Seq[A]
  private[algo] type IndexedSeq[+A] = scala.collection.IndexedSeq[A]
}
