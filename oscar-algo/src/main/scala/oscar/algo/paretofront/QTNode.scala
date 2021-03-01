/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.algo.paretofront

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class QTNode[U: Numeric] extends ParetoElement[U] {
  
  val nSuccessors = 1 << nObjectives
  val Successors = 0 until nSuccessors
  val NonDomSuccessors = 1 until nSuccessors - 1
  val bestSuccessor = Successors.max
  val worstSuccessor = Successors.min
  val successors: Array[Option[QTNode[U]]] = Array.fill(nSuccessors)(None)

  var father: Option[QTNode[U]] = None
  var kSucc: Int = -1
  
  def successorsToSet: Set[QTNode[U]] = {
    successors.filter(s => s match {
      case Some(suc) => true
      case None => false
    }).map(succ => succ.get).toSet
  }
  
  def detach(): Unit = { // Cannot be applied on root
    val f = father.get
    f.successors(kSucc) = None
    father = None
    kSucc = -1
  }

  def successorship(k: Array[Double]): Int = successorship0(k, 0, 0, false, false)

  private def successorship0(k: Array[Double], d: Int, kSucc: Int, dom: Boolean, ndom: Boolean): Int = {
    if (d == nObjectives) if (dom && !ndom) bestSuccessor else kSucc
    else if (comp(k(d), objectives(d))) successorship0(k, d + 1, (kSucc << 1) + 1, true, ndom)
    else if (k(d) == objectives(d)) successorship0(k, d + 1, kSucc << 1, dom, ndom)
    else successorship0(k, d + 1, kSucc << 1, dom, true)
  }

  override def toString: String = objectives.mkString("QTNode(", ", ", ")")
}
