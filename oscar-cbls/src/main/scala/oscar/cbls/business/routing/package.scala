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
package oscar.cbls.business

import oscar.cbls.business.routing.modeling._
import oscar.cbls.core.computation.Store

/**
 * This package proposes dedicated routing neighborhoods and invariants.
 *
 * it represents the routes as a single sequence and everything in this package relies on the routing assumption, which is stated as follows:
 * - there are v vehicles
 * - They are supposed to start from point of values 0 to v-1L
 * - These values must always be present in the sequence in increasing order
 * - They cannot be included within a moved segment
 *
 * Tt defines a type that is a VRP. it only packs a sequence variable and , which is set at the beginning of all optimization
 */
package object routing
  extends RoutingInvariants
  with CapacityInvariants
  with RoutingExtensions
  with InsertPointAPI
  with OnePointMoveAPI
  with RemovePointAPI
  with RouteExchangeAPI
  with SegmentExchangeAPI
  with ThreeOptAPI
  with TwoOptAPI {

  type VRP = oscar.cbls.business.routing.model.VRP
  def vrp(m: Store,n:Int, v: Int, maxPivotPerValuePercent: Int = 4): VRP =
    new VRP(m, n, v, maxPivotPerValuePercent)

  type TTFMatrix = oscar.cbls.business.routing.model.TTFMatrix
  def ttfMatrix(nodeCount: Int, defaultTTF: PrimitiveTravelTimeFunction): TTFMatrix =
    new TTFMatrix(nodeCount, defaultTTF)

  type TTFConst = oscar.cbls.business.routing.model.TTFConst
  def ttfConst(travelDuration: Int): TTFConst =
    new TTFConst(travelDuration)

  type TTFHistogram = oscar.cbls.business.routing.model.TTFHistogram
  def ttfHistogram(nbSlots: Int, overallDuration: Int): TTFHistogram =
    new TTFHistogram(nbSlots, overallDuration)

  type TTFSegments = oscar.cbls.business.routing.model.TTFSegments
  def ttfSegments(nbPoints: Int, overallDuration: Int): TTFSegments =
    new TTFSegments(nbPoints, overallDuration)

  type PrimitiveTravelTimeFunction = oscar.cbls.business.routing.model.PrimitiveTravelTimeFunction

}


