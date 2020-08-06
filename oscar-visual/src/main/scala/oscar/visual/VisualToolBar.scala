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
package oscar.visual

import javax.swing.{JTextField, JLabel, JToolBar, JButton}
import java.awt.event.ActionEvent

import oscar.visual.shapes.VisualRectangle

class VisualToolBar() extends JToolBar {

  def addButton(label:String,actionOnClick : => Unit): Unit ={
    val button = new JButton(label)
    button.addActionListener((_: ActionEvent) => {
			actionOnClick
		})
		add(button)
  }

	def addLabel(text:String): Unit ={
		val label = new JLabel(text)
		add(label)
	}

	def addTextField(defaultText:String): Unit ={
		val textField = new JTextField(defaultText)
		add(textField)
	}
}

object VisualToolBar{
  	
  def main(args : Array[String]): Unit = {
		val f = VisualFrame("toto")
		val tb = f.createToolBar()
		val d = VisualDrawing(flipped = false)
		val inf = f.createFrame("Drawing")
		inf.add(d)
		f.pack()
		
		tb.addButton("rectangle",{
		  val rect = new VisualRectangle(d, 50, 50, 100, 50)
		  rect.toolTip_$eq("Hello")
		  
		  tb.addButton("make it move",{
			  rect.move(100, 20)
		  })
		})		
	}
}
