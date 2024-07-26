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

package oscar.cbls.algo.generator

import GeneratorUtil._

import scala.collection.mutable
import scala.util.Random

object RoutingGenerator extends RoutingGenerator(0L, 1000L) {}

protected class RoutingGenerator(var minXY: Long, var maxXY: Long) {
  protected var _seed: Long = Random.nextLong()
  protected val rng: Random = new Random(_seed)
  GeneratorUtil.rng.setSeed(_seed)

  /** Return the seed used for random generator. */
  def seed: Long = _seed

  /** Set the seed of random number generator with `s`. */
  def setSeed(s: Long): Unit = {
    rng.setSeed(s)
    _seed = s
    GeneratorUtil.rng.setSeed(s)
  }

  def randomDepot: (Long, Long) =
    randomPosition(minXY, maxXY, minXY, maxXY)

  def centerDepot: (Long, Long) = {
    val center: Long = (minXY + maxXY) / 2
    (center, center)
  }

  def randomCities(n: Int): Array[(Long, Long)] =
    Array.fill(n)(randomPosition(minXY, maxXY, minXY, maxXY))

  def clusteredCities(
    numCluster: Int,
    citiesByCluster: Int,
    clusterRadius: Int
  ): Array[(Long, Long)] = {
    val citiesPositions: mutable.Queue[(Long, Long)] = mutable.Queue()

    for (_ <- 0 until numCluster) {
      var currentCenter = randomPosition(minXY, maxXY, minXY, maxXY)
      citiesPositions += currentCenter
      for (_ <- 1 until citiesByCluster) {
        var pos: (Long, Long) = (0L, 0L)
        var tries: Int        = 0
        do {
          pos = randomPosition(minXY, maxXY, minXY, maxXY)
          tries += 1
        } while (distance(currentCenter, pos) > clusterRadius && tries < 10000)
        citiesPositions += pos

        tries = 0
        var newCenter = (0L, 0L)
        do {
          newCenter = randomPosition(minXY, maxXY, minXY, maxXY)
          tries += 1
        } while (distance(currentCenter, newCenter) <= clusterRadius && tries < 10000)
        currentCenter = newCenter
      }
    }
    citiesPositions.toArray
  }

  def distancesMatrix(pos: Array[(Long, Long)]): Array[Array[Long]] =
    Array.tabulate(pos.length, pos.length)((i, j) => distance(pos(i), pos(j)))
}
