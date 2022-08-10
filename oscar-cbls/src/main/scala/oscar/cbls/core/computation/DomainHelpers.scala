package oscar.cbls.core.computation

/**
  * this object provides a few methods that perform safe operzations that do not overflow.
  * in case of overflow, the returned value is set to Min or MaxValue, depending on the considered operation.
  */
object IntDomainHelper{

  // def safeAdd(a:Long, b:Long):Long = {
  // }

  def safeAdd(a: Int, b: Int):Int = {
    val tmp = a + b

    if (a > 0 && b > 0 && tmp < 0) Int.MaxValue
    else if (a <0 && b <0 && tmp > 0) Int.MinValue
    else tmp
  }

  // def safeMul(a:Long, b:Long):Long = {
  //   val tmp = a.toLong * b.toLong

  //   if (a > 0 && b > 0 && tmp < 0) Long.MaxValue
  //   else if (a < 0 && b < 0 && tmp > 0) Long.MaxValue
  //   else if (a < 0 && b > 0 && tmp > 0) Long.MinValue
  //   else if (a > 0 && b < 0 && tmp > 0) Long.MinValue
  //   else tmp
  // }

  def safeMul(a:Int, b:Int):Int = {
    val tmp = a * b

    if (a > 0 && b > 0 && tmp < 0) Int.MaxValue
    else if (a < 0 && b < 0 && tmp > 0) Int.MaxValue
    else if (a < 0 && b > 0 && tmp > 0) Int.MinValue
    else if (a > 0 && b < 0 && tmp > 0) Int.MinValue
    else tmp
  }
}

object LongDomainHelper {
  //Safe addition
  def safeAdd(x: Long, y: Long): Long = {
    if (y > 0 && x + y < x)
      Long.MaxValue
    else {
      if (y < 0 && x + y > x)
        Long.MinValue
      else
        x + y
    }
  }

  //Safe substraction
  def safeSub(x: Long, y: Long): Long = {
    if (y > 0 && x - y > x)
      Long.MinValue
    else {
      if (y < 0 && x - y < x)
        Long.MaxValue
      else
        x - y
    }
  }

  //Safe multiplication
  def safeMult(x: Long, y: Long): Long = {
    val res = x * y
    if (x != 0 && y != res / x) {
      if ((x > 0 && y > 0) || (x < 0 && y < 0))
        Long.MaxValue
      else
        Long.MinValue
    } else
        res
  }



  //Safe multiplication
  def safePow(x: Long, y: Long): Long = {
    val xD = x.toDouble
    val yD = y.toDouble
    val powXY = Math.pow(xD, yD)
    if (powXY.toLong > Long.MaxValue/10L || powXY.isInfinity) {
      Long.MaxValue/10L
    } else {
      powXY.toLong
    }
  }

  def getMinDiv(left: IntValue, right: IntValue): Long = {
    val maxVal = if (right.max == 0L) { -1L } else { right.max }
    val minVal = if (right.min == 0L) { 1L } else { right.min }
    Math.min(left.min / maxVal, Math.min(left.min / minVal,
      Math.min(left.max / maxVal, left.max / minVal)))
  }

  def getMaxDiv(left: IntValue, right: IntValue): Long = {
    val maxVal = if (right.max == 0L) { -1L } else { right.max }
    val minVal = if (right.min == 0L) { 1L } else { right.min }
    Math.max(left.min / maxVal, Math.max(left.min / minVal,
      Math.max(left.max / maxVal, left.max / minVal)))
  }

  def getMinProd2(left: IntValue, right: IntValue): Long = {
    Math.min(safeMult(left.min, right.min), Math.min(safeMult(left.min, right.max),
      Math.min(safeMult(left.max, right.min), safeMult(left.max, right.max))))
  }

  def getMinProd(lm:Long,lM:Long,rm:Long,rM:Long): Long = {
    Math.min(safeMult(lm, rm), Math.min(safeMult(lm, rM), Math.min(safeMult(lM,rm), safeMult(lM,rM))))
  }

  def getMaxProd2(left: IntValue, right: IntValue): Long = {
    Math.max(safeMult(left.min, right.min), Math.max(safeMult(left.min, right.max),
      Math.max(safeMult(left.max, right.min), safeMult(left.max, right.max))))
  }

  def getMaxProd(lm:Long,lM:Long,rm:Long,rM:Long): Long = {
    Math.max(safeMult(lm, rm), Math.max(safeMult(lm, rM), Math.max(safeMult(lM,rm), safeMult(lM,rM))))
  }



}


object DomainHelper2 {

  // Unfortunately all of these options need to be checked. For example if left has the domain -1L0..0 and right has the domain 3..5 then
  // the min value would be -50L and the max value would be 0. But if the domains were -1L0..0 and -1L0..0 then the min would be 0L and max 10L0.
  // So basically all combinations of the domains min and max could yield the new min and max, as the ugly code below indicates. 
  // def isSafeAdd(x: IntValue, y:IntValue): Boolean = {
  //   x.max + y.max <= Long.MaxValue && x.min + y.min >= Long.MinValue
  // }

  // def isSafeSub(x: IntValue, y:IntValue): Boolean = {
  //   x.max - y.min <= Long.MaxValue && x.min - y.max >= Long.MinValue
  // }

  // def isSafeMult(x:IntValue,y:IntValue): Boolean = {
  //   val m1 = x.max * y.max
  //   val m2 = x.max * y.min
  //   val m3 = x.min * y.max
  //   val m4 = x.min * y.min
  //   math.max(math.max(m1,m2), math.max(m3,m4)) <= Long.MaxValue && math.min(math.min(m1,m2), math.min(m3,m4)) >= Long.MinValue
  // }

  // def isSafePow(x: IntValue, y:IntValue): Boolean = {
  //   Math.pow(x.max.toDouble, y.max.toDouble).toLong <= Long.MaxValue/10L
  // }

 

  //Division of integers is always safe.
}


