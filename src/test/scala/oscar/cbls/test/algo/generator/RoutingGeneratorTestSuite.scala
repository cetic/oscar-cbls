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
import oscar.cbls.algo.generator.RoutingGenerator

import scala.math.{pow, round, sqrt}
import scala.util.Random

class RoutingGeneratorTestSuite extends AnyFunSuite with Matchers {

  test("Nodes are well clustered using clustered generator") {
    val numCluster     = 5
    val nodesByCluster = 10
    val r              = 50
    val numDepot       = 3
    val seed = Random.nextLong()
    println(s"Seed: $seed")
    val rng = new Random(seed)

    val nodes = RoutingGenerator.clusteredNodes(numDepot, numCluster, nodesByCluster, r, rng)
    val dist  = RoutingGenerator.distancesMatrix(nodes)

    nodes should have length numDepot + numCluster * nodesByCluster

    for (i <- 0 until numDepot) {
      for (j <- i + 1 until numDepot) {
        dist(i)(j) should be <= round(sqrt(2.0 * pow(2 * r.toDouble, 2.0))) // The biggest distance
        // in a square is the diagonal.
      }
    }

    for (n <- numDepot until numDepot + numCluster * nodesByCluster by nodesByCluster) {
      for (i <- n until n + nodesByCluster) {
        for (j <- i + 1 until n + nodesByCluster) {
          dist(i)(j) should be <= round(sqrt(2.0 * pow(2 * r.toDouble, 2.0)))
        }
      }
    }
  }

  test("Nodes are evenly space using evenlySpacedGenerator") {
    val nodesDist = 50L
    val numNodes  = 100
    val numDepot  = 1
    val seed = Random.nextLong()
    println(s"Seed: $seed")
    val rng = new Random(seed)
    val firstDepot = (42L, 42L)

    val nodes = RoutingGenerator.evenlySpacedNodes(numDepot, numNodes, nodesDist, firstDepot, rng)

    nodes should have length numDepot + numNodes
    // Nodes are generated from the first depot and evenly spaced. So the modulo of their
    // coordinates must be the same than the modulo of the first depot.
    for (n <- nodes) {
      n._1 % nodesDist should equal(firstDepot._1 % nodesDist)
      n._2 % nodesDist should equal(firstDepot._2 % nodesDist)
    }
  }

  test(
    "The expected number of nodes and the distance between them are too big for the " +
      "evenlySpacedGenerator"
  ) {
    val nodesDist = 500L
    val numNodes  = 100
    val seed = Random.nextLong()
    println(s"Seed: $seed")
    val rng = new Random(seed)
    val center = RoutingGenerator.centerDepot

    val nodes = RoutingGenerator.evenlySpacedNodes(1, numNodes, nodesDist, center, rng)

    nodes should have length 9
    // Nodes are generated from the center and evenly spaced. So the modulo of their coordinate
    // must be the same than the modulo of the center.
    for (n <- nodes) {
      n._1 % nodesDist should equal(center._1 % nodesDist)
      n._2 % nodesDist should equal(center._2 % nodesDist)
    }
  }

}
