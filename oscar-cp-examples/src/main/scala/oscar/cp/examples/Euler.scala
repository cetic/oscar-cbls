package oscar.cp.examples


import java.awt.Color

import oscar.cp._
import oscar.visual._
import oscar.visual.shapes.{VisualCircle, VisualLine, VisualRectangle}

/**
 * Euler Problem, a knight must visit every position of a chess board once and come back to its initial position
 * using only valid knight moves.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object Euler extends CPModel with App {

  //  -----------visualization of the euler tour ----------
  val f = new VisualFrame("Euler", 1, 1)
  val drawing = new VisualDrawing(false, true)
  f.createFrame("Euler Tour").add(drawing)
  val scale = 100

  val n = 6 // multiple of two

  def reachables(i: Int): Set[Int] = {
    val l = i % n
    val c = i / n

    (for ((dl,dc) <- Seq((1,2),(2,1),(-1,2),(-2,1),(1,-2),(2,-1),(-1,-2),(-2,-1))) yield {
      ((l+dl),(c+dc))
    }).filter{case(l,c) => l >= 0 && l < n && c >= 0 && c < n}.map{case(a,b) => b*n + a}.toSet

  }

  val x = (0 until n*n).map(v => CPIntVar(reachables(v)))

  add(circuit(x))

  search {
    binaryFirstFail(x)
  } onSolution {
    println(x.map(_.value).mkString(","))
    for (i <- 0 until n; j <- 0 until n) {
      val rect = new VisualRectangle(drawing, i * scale, j * scale, scale, scale)
      if (i % 2 == 0 && j % 2 == 0) rect.innerCol = Color.gray
      else if (i % 2 == 1 && j % 2 == 1) rect.innerCol = Color.gray
    }
    for (i <- 0 until n*n) {
      val v = x(i).value
      val (c, l) = (v / n, v % n)
      new VisualCircle(drawing, scale / 2 + (i / n) * scale, scale / 2 + (i % n) * scale, 6).innerCol = Color.RED
      VisualLine(drawing, scale / 2 + (i / n) * scale, scale / 2 + (i % n) * scale, scale / 2 + c * scale, scale / 2 + l * scale)
    }
    f.pack()
    drawing.repaint()
  }

  val stats = start(1)

  println(stats)
}
