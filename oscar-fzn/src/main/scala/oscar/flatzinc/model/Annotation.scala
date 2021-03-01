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
/**
 * @author Leonard Debroux
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.model

class
Annotation (
    val name: String,
    var args: List[Any]
	){
    def this(name: String) = this(name,List.empty[Any]);
    def add(e: Any): Unit ={
      args = args ++ List(e);
    }
	override def toString() = name + "(" + args.mkString(", ") + ")"
}
