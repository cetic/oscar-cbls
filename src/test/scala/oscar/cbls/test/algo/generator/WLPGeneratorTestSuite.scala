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

package oscar.cbls.test.algo.generator

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.generator.WarehouseLocationGenerator

import scala.math.{pow, round, sqrt}
import scala.util.Random

class WLPGeneratorTestSuite extends AnyFunSuite with Matchers {

  test("Two generators with the same seed generate the same data") {
    val (open1, wp1, dp1, cd1, wd1) =
      WarehouseLocationGenerator.generateRandomWLP(5, 20, seed = 42L)
    val (open2, wp2, dp2, cd2, wd2) =
      WarehouseLocationGenerator.generateRandomWLP(5, 20, seed = 42L)

    open1 should equal(open2)
    wp1 should equal(wp2)
    dp1 should equal(dp2)
    cd1 should equal(cd2)
    wd1 should equal(wd2)
  }

  test("Warehouses are well distributed using grid generator.") {
    val seed = Random.nextLong()
    val rng = new Random(seed)
    println(s"Seed: $seed")
    WarehouseLocationGenerator.setMapDimensions(0L, 120L)
    val wp = WarehouseLocationGenerator.gridWarehousesPositions(8, 2L, rng)

    for (i <- wp.indices) {
      i % 4 match {
        case 0 =>
          wp(i)._1 should (be >= 0L and be <= 60L)
          wp(i)._2 should (be >= 0L and be <= 60L)
        case 1 =>
          wp(i)._1 should (be >= 0L and be <= 60L)
          wp(i)._2 should (be >= 60L and be <= 120L)
        case 2 =>
          wp(i)._1 should (be >= 60L and be <= 120L)
          wp(i)._2 should (be >= 0L and be <= 60L)
        case 3 =>
          wp(i)._1 should (be >= 60L and be <= 120L)
          wp(i)._2 should (be >= 60L and be <= 120L)
      }
    }
  }

  test("Warehouses are enough far from each other using min dist generator.") {
    val seed = Random.nextLong()
    val rng = new Random(seed)
    println(s"Seed: $seed")
    val wp = WarehouseLocationGenerator.minDistWarehouses(10, 20L, rng)
    for (i <- wp.indices) {
      for (j <- i + 1 until wp.length) {
        round(
          sqrt(pow((wp(i)._1 - wp(j)._1).toDouble, 2.0) + pow((wp(i)._2 - wp(j)._2).toDouble, 2.0))
        ) should be > 20L
      }
    }
  }
}
