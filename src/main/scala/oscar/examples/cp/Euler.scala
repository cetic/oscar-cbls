/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.examples.cp


import oscar.cp.modeling._
import oscar.search._
import oscar.visual._
import java.awt.Color

/**
 * 
 * Euler Problem, a knight must visit every position of a chess board once and come back to its initial position
 * using only valid knight moves.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object Euler  extends CPModel {
	def main(args: Array[String]) {

		def reachables(i : Int) : Set[Int] = {
			def onBoard (y : Int*) : Set[Int] = y.filter(x=> x>=0 && x<=63).toSet
			i%8 match {
				case 0 =>  onBoard(i-15,i-6,i+10,i+17)
				case 1 =>  onBoard(i-17,i-15,i-6,i+10,i+15,i+17)
				case 6 =>  onBoard(i-17,i-15,i-10,i+6,i+15,i+17)
				case 7 =>  onBoard(i-17,i-10,i+6,i+15)
				case _ =>  onBoard(i-17,i-15,i-10,i-6,i+6,i+10,i+15,i+17)
			}
		}
		
		val cp = new CPSolver()
		val x = (0 until 64).map(v => CPVarInt(cp,reachables(v)))
		
		cp.solve subjectTo {
			cp.add(circuit(x))
		} exploration {
		  cp.binaryFirstFail(x)
		  println(x.map(_.getValue).mkString(","))
		}
		
		cp.printStats()
		
		//  -----------visualization of the euler tour ----------
	
		val f = new VisualFrame("Euler",1,1)
		val drawing = new VisualDrawing(false)
		f.createFrame("Euler Tour").add(drawing)
		val scale = 100
		
		for (i <- 0 until 8; j <- 0 until 8) {
		  val rect = new VisualRectangle(drawing,i*scale,j*scale,scale,scale)
		  if (i % 2 == 0) {
		    if (j % 2 == 0) rect.setInnerCol(Color.gray)
		  } else {
		    if (j % 2 == 1) rect.setInnerCol(Color.gray)
		  }	
		}		
		for (i <- 0 until 64) {
		  val v = x(i).getValue()
		  val (c,l) = (v/8, v%8)
		  new VisualCircle(drawing,scale/2+(i/8)*scale,scale/2+(i%8)*scale,3).setInnerCol(Color.RED)
		  new VisualLine(drawing,scale/2+(i/8)*scale,scale/2+(i%8)*scale,scale/2+c*scale,scale/2+l*scale)
		}
		f.pack()
		drawing.repaint()
	

	}

}