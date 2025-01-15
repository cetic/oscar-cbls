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

package oscar.cbls.algo.dag

case class DAGCycleException(message: String) extends Exception {
  override def getMessage: String = s"DAG exception ==> Cycle detected: $message"
}

case class DAGIncoherenceException(message: String) extends Exception {
  override def getMessage: String = s"DAG exception ==> Graph is incoherent: $message"
}

case class DAGUniqueIDException(message: String) extends Exception {
  override def getMessage: String = s"DAG exception ==> Unique ID already set: $message"
}
