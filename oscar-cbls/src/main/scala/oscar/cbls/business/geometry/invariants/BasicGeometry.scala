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
package oscar.cbls.business.geometry.invariants

import org.locationtech.jts.operation.buffer.BufferOp
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.model._
import oscar.cbls.core.computation.{AtomicValue, CBLSIntVar, ChangingAtomicValue, ChangingIntValue, Domain, IntInvariant, IntNotificationTarget, IntValue, Invariant, Store, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker

case class IsWithin(inner:AtomicValue[GeometryValue], outer:AtomicValue[GeometryValue])
  extends Invariant with Constraint with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(inner)
  this.registerStaticAndDynamicDependency(outer)
  registerConstrainedVariables(inner, outer)

  finishInitialization()

  override val violation = new CBLSIntVar(model,0,0 to Int.MaxValue)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    violation := inner.value.geometry.difference(outer.value.geometry).getArea.toInt
  }

  override def violation(v: Value):IntValue = { if (inner == v || inner == v) violation else 0 }

  override def checkInternals(c: Checker): Unit = {}
}

class Union(store:Store,a:AtomicValue[GeometryValue],b:AtomicValue[GeometryValue])
  extends CBLSGeometryInvariant(store:Store,
    initialValue = GeometryValue(a.value.geometry union b.value.geometry))
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := GeometryValue(a.value.geometry union b.value.geometry)
  }
}

case class Intersection(store:Store,a:AtomicValue[GeometryValue],b:AtomicValue[GeometryValue])
  extends CBLSGeometryInvariant(store:Store,geometry.emptyGeometryValue)
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)
  finishInitialization(store)

  performInvariantPropagation()

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this :=  (if (!(a.value mightOverlapBasedOnOverApproximatingValues b.value))
      geometry.emptyGeometryValue
    else new GeometryValue(a.value.geometry intersection b.value.geometry))
  }
}

class ConvexHull(store:Store,a:AtomicValue[GeometryValue])
  extends CBLSGeometryInvariant(store:Store,
    initialValue = new GeometryValue(a.value.geometry.convexHull()))
    with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := GeometryValue(a.value.geometry.convexHull())
  }
}

case class Area(store:Store,a:AtomicValue[GeometryValue])
  extends IntInvariant(
    initialValue = a.value.geometry.getArea().toLong,
    initialDomain = Domain(0L,Long.MaxValue))
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this :=  a.value.geometry.getArea().toLong
  }
}

case class Length(store:Store,a:AtomicValue[GeometryValue])
  extends IntInvariant(
    initialValue = a.value.geometry.getEnvelope.getLength.toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(a)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := a.value.geometry.getEnvelope.getLength.toInt
  }
}

class Centroid(store:Store,shape:AtomicValue[GeometryValue])
  extends CBLSGeometryInvariant(
    store,
    initialValue = {
      val c = shape.value.geometry.getCentroid
      GeometryValue(
        c,
        inputCentreOfOverApproximatingCircle = Some(c),
        inputOverApproximatingRadius= Some(0.0))
    }
  ) with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(shape)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    val c = shape.value.geometry.getCentroid
    this := GeometryValue(
      c,
      inputCentreOfOverApproximatingCircle = Some(c),
      inputOverApproximatingRadius = Some(0.0))
  }
}

class DistanceBetweenCentroids(store:Store,pointA:AtomicValue[GeometryValue],pointB:AtomicValue[GeometryValue])
  extends IntInvariant(
    initialValue = pointA.value.geometry.getCentroid.distance(pointA.value.geometry.getCentroid).toInt,
    initialDomain = 0 to Int.MaxValue)
    with GeometryNotificationTarget{

  this.registerStaticAndDynamicDependency(pointA)
  this.registerStaticAndDynamicDependency(pointB)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := pointA.value.geometry.getCentroid.distance(pointA.value.geometry.getCentroid).toInt
  }
}

class ResizableCircle(store:Store, size: IntValue, givenName: String = null)
  extends CBLSGeometryInvariant(store, {
    val circle = geometry.createCircle(size.value, nbEdges = 36)
    circle
  }) with IntNotificationTarget {

  this.registerStaticAndDynamicDependency(size)
  finishInitialization(store)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit = {
    require(newVal > 0, "The size of a circle has a positive value")
    if(oldVal != newVal)
      this := geometry.createCircle(newVal, nbEdges = 36)
  }

  override def toString: String = if (givenName == null) value.toString else givenName
}

class ResizableSquare(store:Store, size: IntValue, givenName: String = null)
  extends CBLSGeometryInvariant(store, {
    val square = geometry.createRectangle(size.value, size.value)
    square
  }) with IntNotificationTarget {

  this.registerStaticAndDynamicDependency(size)
  finishInitialization(store)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit = {
    require(newVal > 0, "The size of a square has a positive value")
    if(oldVal != newVal)
      this := geometry.createRectangle(newVal, newVal)
  }

  override def toString: String = if (givenName == null) value.toString else givenName
}

class ResizableRectangle(store:Store, height: IntValue, width: IntValue, givenName: String = null)
  extends CBLSGeometryInvariant(store, {
    val square = geometry.createRectangle(height.value, width.value)
    square
  }) with IntNotificationTarget {

  this.registerStaticAndDynamicDependency(height)
  this.registerStaticAndDynamicDependency(width)
  finishInitialization(store)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit = {
    require(newVal > 0, "The height and width of a rectangle have a positive value")
    if(oldVal != newVal)
      this := geometry.createRectangle(height.value, width.value)
  }

  override def toString: String = if (givenName == null) value.toString else givenName
}

class Buffer(store: Store, shape: AtomicValue[GeometryValue], distance: Long, givenName: String = null, nbSegments:Int = 8)
  extends CBLSGeometryInvariant(store, {
    val bufferedGeometry = BufferOp.bufferOp(shape.value.geometry, distance, nbSegments)
    GeometryValue(bufferedGeometry)
  }) with GeometryNotificationTarget {

  this.registerStaticAndDynamicDependency(shape)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue], id: Int, oldVal: GeometryValue, newVal: GeometryValue): Unit = {
    val bufferedGeometry = BufferOp.bufferOp(newVal.geometry, distance, nbSegments)
    this := GeometryValue(bufferedGeometry)
  }

  override def toString: String = if(givenName == null) value.toString else givenName
}
