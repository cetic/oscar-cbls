package oscar.cbls.algo.rb

import scala.annotation.tailrec

//le booléen: true le noeud a déjà été montré (dans un parcour gauche à droite)
class RedBlackTreeMapExplorer[@specialized(Int) V](position: QList[(T[V], Boolean)]) {
  def key: Int = position.head._1.pk
  def value: V = position.head._1.pv.get

  override def toString: String =
    "RBPosition(key:" + key + " value:" + value + " stack:" + position.toList + ")"

  def next: Option[RedBlackTreeMapExplorer[V]] = {

    @tailrec
    def unstack1(position: QList[(T[V], Boolean)]): QList[(T[V], Boolean)] = {
      if (position == null) return null
      val head = position.head
      if (!head._2) {
        // not presented yet, so we present this one
        QList((head._1, true), position.tail)
      } else {
        // already presented, so unstack
        unstack1(position.tail)
      }
    }

    @tailrec
    def descendToLeftMost(position: QList[(T[V], Boolean)]): QList[(T[V], Boolean)] = {
      val headTree = position.head._1
      headTree.pl match {
        case t: T[V] => descendToLeftMost(QList((t, false), position))
        case _       => QList((headTree, true), position.tail)
      }
    }

    val newStack = position.head._1.pr match {
      case t: T[V] => descendToLeftMost(QList((t, false), position))
      case _       => unstack1(position)
    }

    if (newStack == null) None
    else Some(new RedBlackTreeMapExplorer[V](newStack))
  }

  def prev: Option[RedBlackTreeMapExplorer[V]] = {
    @tailrec
    def unstack1(position: QList[(T[V], Boolean)]): QList[(T[V], Boolean)] = {
      if (position == null) return null
      val head = position.head
      if (head._2) {
        // already presented, so roll back to it.
        QList((head._1, true), position.tail)
      } else {
        // already presented, so unstack
        unstack1(position.tail)
      }
    }

    @tailrec
    def descendToRightMost(position: QList[(T[V], Boolean)]): QList[(T[V], Boolean)] = {
      val headTree = position.head._1
      headTree.pr match {
        case t: T[V] => descendToRightMost(QList((t, true), position))
        case _       => QList((headTree, true), position.tail)
      }
    }

    val newStack = position.head._1.pl match {
      case t: T[V] =>
        descendToRightMost(QList((t, true), QList((position.head._1, false), position.tail)))
      case _ => unstack1(position.tail)
    }

    if (newStack == null) None
    else {
      assert(
        new RedBlackTreeMapExplorer[V](newStack).next.head.key == this.key,
        "prev.next.key != this.key; this:" + this + " prev:" + new RedBlackTreeMapExplorer[V](
          newStack
        )
      )
      Some(new RedBlackTreeMapExplorer[V](newStack))
    }
  }
}
