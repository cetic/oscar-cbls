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

object DAGException {
  final def cycle(details: String): DAGException =
    DAGException(s"Cycle detected : $details")
  final def graphIncoherence(details: String): DAGException =
    DAGException(s"Graph is incoherent : $details")
  final def uniqueIDAlreadySet(details: String): DAGException =
    DAGException(s"Unique ID already set : $details")
}

case class DAGException(message: String) extends Exception {
  override def getMessage: String = s"Propagation exception ==> $message"
}
