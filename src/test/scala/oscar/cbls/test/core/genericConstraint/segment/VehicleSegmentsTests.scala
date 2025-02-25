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

package oscar.cbls.test.core.genericConstraint.segment

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.genericConstraint.segment._
import oscar.cbls.modeling.routing.VRS

class VehicleSegmentsTests extends AnyFunSuite with Matchers {

  test("Vehicle of segment is correctly initialized") {
    val model = new Store()
    val vrs   = VRS(model, 10, 1)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 5))
    model.propagate()

    val segmentsOfVehicle = VehicleSegments(vrs, 0)
    segmentsOfVehicle.segments must have length 1

    val seg = segmentsOfVehicle.segments.head
    seg must equal(PrecomputedSubSequence(0, 5, 6))

  }

  test("Removes one point") {
    val model = new Store()
    val vrs   = VRS(model, 10, 1)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 5))
    model.propagate()
    val segmentsOfVehicle = VehicleSegments(vrs, 0)
    val toRemoveExp       = vrs.routes.value().explorerAtAnyOccurrence(3).get

    val (newSegments, removedSegments) =
      segmentsOfVehicle.removeNode(toRemoveExp, 0)

    newSegments.segments must have length 2
    removedSegments must have length 1

    val firstSeg   = newSegments.segments.head
    val secondSeg  = newSegments.segments.tail.head
    val removedSeg = removedSegments.head

    firstSeg must equal(PrecomputedSubSequence(0, 2, 3))
    secondSeg must equal(PrecomputedSubSequence(4, 5, 2))
    removedSeg must equal(PrecomputedSubSequence(3, 3, 1))
  }

  test("Inserts a new node") {
    val model = new Store()
    val vrs   = VRS(model, 10, 1)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 5))
    model.propagate()
    val segmentsOfVehicle = VehicleSegments(vrs, 0)
    val insertAfterExp    = vrs.routes.value().explorerAtAnyOccurrence(3).get

    val newSegments = segmentsOfVehicle.insertNode(7, insertAfterExp, 0)

    newSegments.segments must have length 3
    newSegments.segments must contain inOrderOnly (
      PrecomputedSubSequence(0, 3, 4),
      NewNode(7),
      PrecomputedSubSequence(4, 5, 2)
    )
  }

  test("Inserts two adjacent new nodes") {
    val model = new Store()
    val vrs   = VRS(model, 10, 1)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 5))
    model.propagate()
    var segmentsOfVehicle = VehicleSegments(vrs, 0)
    var insertAfterExp    = vrs.routes.value().explorerAtAnyOccurrence(3).get
    vrs.routes.insertAfterPosition(7, insertAfterExp)
    segmentsOfVehicle = segmentsOfVehicle.insertNode(7, insertAfterExp, 0)

    insertAfterExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(7).get
    vrs.routes.insertAfterPosition(9, insertAfterExp)
    segmentsOfVehicle = segmentsOfVehicle.insertNode(9, insertAfterExp, 0)

    model.propagate()

    segmentsOfVehicle.segments must have length 4
    segmentsOfVehicle.segments must contain inOrderOnly (
      PrecomputedSubSequence(0, 3, 4),
      NewNode(7),
      NewNode(9),
      PrecomputedSubSequence(4, 5, 2)
    )
  }

  test("Moves segments on the same vehicle") {
    val model = new Store()
    val vrs   = VRS(model, 20, 1)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 15))
    model.propagate()
    var segmentsOfVehicle = VehicleSegments(vrs, 0)

    var toRemoveExp = vrs.routes.value().explorerAtAnyOccurrence(3).get
    vrs.routes.remove(toRemoveExp)
    segmentsOfVehicle = segmentsOfVehicle.removeNode(toRemoveExp, 0)._1

    toRemoveExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(10).get
    vrs.routes.remove(toRemoveExp)
    segmentsOfVehicle = segmentsOfVehicle.removeNode(toRemoveExp, 0)._1

    var insertAfterExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(5).get
    vrs.routes.insertAfterPosition(17, insertAfterExp)
    segmentsOfVehicle = segmentsOfVehicle.insertNode(17, insertAfterExp, 0)

    insertAfterExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(7).get
    vrs.routes.insertAfterPosition(16, insertAfterExp)
    segmentsOfVehicle = segmentsOfVehicle.insertNode(16, insertAfterExp, 0)

    val fromExp = vrs.routes.pendingValue.explorerAtPosition(2).get
    val toExp   = vrs.routes.pendingValue.explorerAtPosition(8).get
    insertAfterExp = vrs.routes.pendingValue.explorerAtPosition(12).get
    vrs.routes.move(fromExp, toExp, insertAfterExp, flip = false)
    val (sourceSeg, targetSeg) = VehicleSegments.moveSegments(
      0,
      0,
      segmentsOfVehicle,
      segmentsOfVehicle,
      fromExp,
      toExp,
      insertAfterExp,
      flip = false
    )

    model.propagate()

    sourceSeg.segments must equal(targetSeg.segments)
    sourceSeg.segments must have length 9
    sourceSeg.segments must contain inOrderOnly (
      PrecomputedSubSequence(0, 1, 2),
      PrecomputedSubSequence(8, 9, 2),
      PrecomputedSubSequence(11, 12, 2),
      PrecomputedSubSequence(2, 2, 1),
      PrecomputedSubSequence(4, 5, 2),
      NewNode(17),
      PrecomputedSubSequence(6, 7, 2),
      NewNode(16),
      PrecomputedSubSequence(13, 15, 3)
    )

  }

  test("Moves and flips segments on the same vehicle") {
    val model = new Store()
    val vrs   = VRS(model, 20, 1)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 15))
    model.propagate()
    var segmentsOfVehicle = VehicleSegments(vrs, 0)

    var toRemoveExp = vrs.routes.value().explorerAtAnyOccurrence(3).get
    vrs.routes.remove(toRemoveExp)
    segmentsOfVehicle = segmentsOfVehicle.removeNode(toRemoveExp, 0)._1

    toRemoveExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(10).get
    vrs.routes.remove(toRemoveExp)
    segmentsOfVehicle = segmentsOfVehicle.removeNode(toRemoveExp, 0)._1

    var insertAfterExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(5).get
    vrs.routes.insertAfterPosition(17, insertAfterExp)
    segmentsOfVehicle = segmentsOfVehicle.insertNode(17, insertAfterExp, 0)

    insertAfterExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(7).get
    vrs.routes.insertAfterPosition(16, insertAfterExp)
    segmentsOfVehicle = segmentsOfVehicle.insertNode(16, insertAfterExp, 0)

    val fromExp = vrs.routes.pendingValue.explorerAtPosition(2).get
    val toExp   = vrs.routes.pendingValue.explorerAtPosition(8).get
    insertAfterExp = vrs.routes.pendingValue.explorerAtPosition(12).get
    vrs.routes.move(fromExp, toExp, insertAfterExp, flip = true)
    val (sourceSeg, targetSeg) = VehicleSegments.moveSegments(
      0,
      0,
      segmentsOfVehicle,
      segmentsOfVehicle,
      fromExp,
      toExp,
      insertAfterExp,
      flip = true
    )

    model.propagate()

    sourceSeg.segments must equal(targetSeg.segments)
    sourceSeg.segments must have length 9
    sourceSeg.segments must contain inOrderOnly (
      PrecomputedSubSequence(0, 1, 2),
      PrecomputedSubSequence(8, 9, 2),
      PrecomputedSubSequence(11, 12, 2),
      NewNode(16),
      FlippedPreComputedSubSequence(7, 6, 2),
      NewNode(17),
      FlippedPreComputedSubSequence(5, 4, 2),
      FlippedPreComputedSubSequence(2, 2, 1),
      PrecomputedSubSequence(13, 15, 3)
    )

  }

  test("Move segments to another vehicle") {
    val model = new Store()
    val vrs   = VRS(model, 50, 2)
    model.close()
    vrs.routes := IntSequence(List.from(0 to 20 by 2) ::: List.from(1 to 21 by 2))
    model.propagate()
    var segmentsOfVehicle0 = VehicleSegments(vrs, 0)
    var segmentsOfVehicle1 = VehicleSegments(vrs, 1)

    var toRemoveExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(6).get
    vrs.routes.remove(toRemoveExp)
    segmentsOfVehicle0 = segmentsOfVehicle0.removeNode(toRemoveExp, 0)._1

    toRemoveExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(16).get
    vrs.routes.remove(toRemoveExp)
    segmentsOfVehicle0 = segmentsOfVehicle0.removeNode(toRemoveExp, 0)._1

    var insertAfterExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(10).get
    vrs.routes.insertAfterPosition(42, insertAfterExp)
    segmentsOfVehicle0 = segmentsOfVehicle0.insertNode(42, insertAfterExp, 0)

    val posOf1 = vrs.routes.pendingValue.positionOfAnyOccurrence(1).get

    toRemoveExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(7).get
    vrs.routes.remove(toRemoveExp)
    segmentsOfVehicle1 = segmentsOfVehicle1.removeNode(toRemoveExp, posOf1)._1

    toRemoveExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(15).get
    vrs.routes.remove(toRemoveExp)
    segmentsOfVehicle1 = segmentsOfVehicle1.removeNode(toRemoveExp, posOf1)._1

    val fromExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(4).get
    val toExp   = vrs.routes.pendingValue.explorerAtAnyOccurrence(18).get
    insertAfterExp = vrs.routes.pendingValue.explorerAtAnyOccurrence(11).get
    vrs.routes.move(fromExp, toExp, insertAfterExp, flip = false)
    val (newSeg0, newSeg1) = VehicleSegments.moveSegments(
      0,
      posOf1,
      segmentsOfVehicle0,
      segmentsOfVehicle1,
      fromExp,
      toExp,
      insertAfterExp,
      flip = false
    )

    model.propagate()

    newSeg0.segments must have length 2
    newSeg0.segments must contain inOrderOnly (
      PrecomputedSubSequence(0, 2, 2),
      PrecomputedSubSequence(20, 20, 1)
    )

    newSeg1.segments must have length 9
    newSeg1.segments must contain inOrderOnly (
      PrecomputedSubSequence(1, 5, 3),
      PrecomputedSubSequence(9, 11, 2),
      PrecomputedSubSequence(4, 4, 1),
      PrecomputedSubSequence(8, 10, 2),
      NewNode(42),
      PrecomputedSubSequence(12, 14, 2),
      PrecomputedSubSequence(18, 18, 1),
      PrecomputedSubSequence(13, 13, 1),
      PrecomputedSubSequence(17, 21, 3)
    )

    // Test if the route is coherent with the segments decomposition
    var startExp = vrs.routes.value().explorerAtAnyOccurrence(0).get
    for (seg <- newSeg0.segments ::: newSeg1.segments) {
      val start = startExp.value

      start must equal(seg.startNode())

      val endExp = vrs.routes.value().explorerAtPosition(startExp.position + seg.length() - 1).get
      val end    = endExp.value

      end must equal(seg.endNode())
      startExp = endExp.next
    }

  }

}
