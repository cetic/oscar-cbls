package oscar.cbls.business.geometry.invariants


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

import org.locationtech.jts.geom.Point
import org.locationtech.jts.geom.util.AffineTransformation
import oscar.cbls.Store
import oscar.cbls.business.geometry.model._
import oscar.cbls.core.computation._

class Compose(store:Store,
              a:ChangingAtomicValue[AffineTransformationValue],
              b:ChangingAtomicValue[AffineTransformationValue])
  extends CBLSAffineTransformInvariant(
    initialValue = AffineTransformationValue(
      new AffineTransformation(new AffineTransformation(a.value.affineTransform) compose b.value.affineTransform),
      (a.value.uniformScalingFactor,b.value.uniformScalingFactor) match {case (Some(x),Some(y)) => Some(x*y); case _ => None}))
    with AffineTransformNotificationTarget {

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)
  finishInitialization(store)

  override def notifyAffineTransformChange(a: ChangingAtomicValue[AffineTransformationValue],
                                           id: Int,
                                           oldVal: AffineTransformationValue,
                                           newVal: AffineTransformationValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := AffineTransformationValue(
      new AffineTransformation(new AffineTransformation(a.value.affineTransform) compose b.value.affineTransform),
      (a.value.uniformScalingFactor, b.value.uniformScalingFactor)
      match {
        case (Some(x), Some(y)) => Some(x * y)
        case _ => None
      })
  }
}

class Translation(store:Store,x:IntValue,y:IntValue)
  extends CBLSAffineTransformInvariant(
    initialValue = AffineTransformationValue(
      AffineTransformation.translationInstance(x.value.toDouble,y.value.toDouble),
      Some(1.0)))
    with IntNotificationTarget {

  setName("Translation")

  this.registerStaticAndDynamicDependency(x)
  this.registerStaticAndDynamicDependency(y)
  finishInitialization(store)

  override def toString: String = "Translation"

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := AffineTransformationValue(
      AffineTransformation.translationInstance(x.value.toDouble,y.value.toDouble),
      Some(1.0))
  }
}

/**
  * Creates a transformation for a rotation
  * about the origin
  * by an angle <i>theta</i>.
  * Positive angles correspond to a rotation
  * in the counter-clockwise direction.
  * @param store the CBLS store
  * @param theta: the angle of rotation, in t th of degree
  */
class RotationAroundZero(store:Store,theta:IntValue, t:Int = 1)
  extends CBLSAffineTransformInvariant(
    initialValue = AffineTransformationValue(
      AffineTransformation.rotationInstance(theta.value * java.lang.Math.PI / (180 * t)),
      Some(1.0)))
    with IntNotificationTarget {

  setName("Rotation")

  this.registerStaticAndDynamicDependency(theta)
  finishInitialization(store)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := AffineTransformationValue(
      AffineTransformation.rotationInstance(theta.value * java.lang.Math.PI / (180 * t)),
      Some(1.0))
  }
}

class Apply(store:Store,a:AtomicValue[AffineTransformationValue],b:AtomicValue[GeometryValue])
  extends CBLSGeometryInvariant(
    store:Store,
    initialValue = {
      a.value.uniformScalingFactor match{
        case None => GeometryValue(a.value.affineTransform.transform(b.value.geometry))
        case Some(s) =>
          val newGeo = a.value.affineTransform.transform(b.value.geometry)

          val newCentroid = b.value.centerOfOverApproximatingCircleOpt match{
            case None => None
            case Some(c:Point) => Some(a.value.affineTransform.transform(c).asInstanceOf[Point])
          }

          val newRadius = b.value.overApproximatingRadiusOpt match{
            case None => None
            case Some(r) => Some(r*s)
          }

          GeometryValue(newGeo,newCentroid,newRadius)
      }
    }) with GeometryNotificationTarget with AffineTransformNotificationTarget {

  setName(a + "(" + b + ")")

  this.registerStaticAndDynamicDependency(a)
  this.registerStaticAndDynamicDependency(b)
  finishInitialization(store)

  override def notifyGeometryChange(a: ChangingAtomicValue[GeometryValue],
                                    id: Int,
                                    oldVal: GeometryValue,
                                    newVal: GeometryValue): Unit = {
    this.scheduleForPropagation()
  }

  override def notifyAffineTransformChange(a: ChangingAtomicValue[AffineTransformationValue],
                                           id: Int,
                                           oldVal: AffineTransformationValue,
                                           newVal: AffineTransformationValue): Unit = {
    this.scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit = {
    this := {
      a.value.uniformScalingFactor match {
        case None => GeometryValue(a.value.affineTransform.transform(b.value.geometry))
        case Some(s) =>
          val newGeo = a.value.affineTransform.transform(b.value.geometry)

          val newCentroid = b.value.centerOfOverApproximatingCircleOpt match {
            case None => None
            case Some(c: Point) => Some(a.value.affineTransform.transform(c).asInstanceOf[Point])
          }

          val newRadius = b.value.overApproximatingRadiusOpt match {
            case None => None
            case Some(r) => Some(r * s)
          }

          GeometryValue(newGeo,newCentroid, newRadius)
      }
    }
  }
}




