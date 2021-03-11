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

package oscar.modeling.misc

import org.jfree.data.statistics.{SimpleHistogramBin, SimpleHistogramDataset}

import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Double.TotalOrdering

/**
  * Dynamically creates bins to reflect a (probable) gaussian distribution of the data
  * @param key: name of the dataset
  * @param nbBins: number of bins needed. Must be >= 3.
  */
class FixedBinsHistogramDataset(key: String, nbBins: Int) {
  val dataset = new SimpleHistogramDataset(key)
  dataset.setAdjustForBinSize(false)

  val observations = new ArrayBuffer[Double]
  var currentSum = 0.0
  var currentSquareSum = 0.0
  var lastUpdate = 0

  def addObservation(obs: Double): Unit = addObservations(Array(obs))
  def addObservations(obs: Array[Double]): Unit = {
    currentSum = obs.foldLeft(currentSum)((a, b) => a+b)
    currentSquareSum = obs.foldLeft(currentSquareSum)((a,b) => a+b*b)

    observations ++= obs
    lastUpdate += obs.length

    if(lastUpdate >= 100){
      reinit()
    }
    else {
      try {
        dataset.addObservations(obs)
      } catch {
        case e: Exception => reinit()
      }
    }
  }

  def reinit(): Unit = {
    lastUpdate = 0

    val mean = currentSum/observations.size
    val stddev = math.sqrt(currentSquareSum/observations.size - mean*mean)

    val newMin = mean-4*stddev
    val newMax = mean+4*stddev

    if(newMin == newMax) { //nothing to do here
      lastUpdate = observations.size+1
      return
    }

    dataset.removeAllBins()
    for(i <- 0 until nbBins - 2){
      val binMin = newMin+(newMax-newMin)*(i.toDouble/(nbBins-2).toDouble)
      val binMax = newMin+(newMax-newMin)*((i+1).toDouble/(nbBins-2).toDouble)
      dataset.addBin(new SimpleHistogramBin(binMin, binMax, true, false))
    }
    dataset.addBin(new SimpleHistogramBin(newMax, Seq(observations.max,mean+5*stddev).max, true, true))
    dataset.addBin(new SimpleHistogramBin(Seq(observations.min,mean-5*stddev).min, newMin, true, false))
    dataset.addObservations(observations.toArray)
  }

  def getRealDataset: SimpleHistogramDataset = dataset
}
