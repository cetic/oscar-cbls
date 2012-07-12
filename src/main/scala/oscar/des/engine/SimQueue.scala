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

package oscar.des.engine

import oscar.invariants._
import scala.util.continuations._

class SimQueue {

  val isOpen = new Var[Boolean](false)
  val serve = new EventOne[Unit]  
  val isBusy = new Var[Boolean](false)
  val isEmpty = new Var[Boolean](true)
  
  def enter: Boolean@suspendable = {
    if ( ! isOpen() ) false
    else {
      isEmpty := false
      if ( isBusy() )
        waitFor(serve) 
      else
        cpsunit
      isBusy := true
      true
    }
  }
  
  def leave(){
    
    if ( serve.hanging > 0 ){
      serve emit()
    }else{
      isEmpty := true
      isBusy := false
    }
    
  }
  def close(){isOpen := false}
  def open(){isOpen := true}
  
}