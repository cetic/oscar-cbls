// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.util.exceptions

abstract class PropagationExceptions extends Exception{
  override def getMessage: String = s"Propagation exception ==> "
}

case class CycleException(details: String) extends PropagationExceptions {
  override def getMessage: String = super.getMessage ++ s"Cycle detected : $details"
}

case class GraphIncoherenceException(details: String) extends PropagationExceptions {
  override def getMessage: String = super.getMessage ++ s"Graph is incoherent : $details"
}

case class UniqueIDAlreadySetException(details: String) extends PropagationExceptions {
  override def getMessage: String = super.getMessage ++ s"Unique ID already set : $details"
}
