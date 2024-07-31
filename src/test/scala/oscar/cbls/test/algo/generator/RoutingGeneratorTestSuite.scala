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

import scala.util.Random

class RoutingGeneratorTestSuite extends AnyFunSuite with Matchers {

  test("Nodes are well clustered using clustered generator") {
    val numCluster     = 5
    val nodesByCluster = 10
    val r              = 50
    RoutingGenerator.setSeed(Random.nextLong())
    println(s"Seed: ${RoutingGenerator.seed}")

    val nodes = RoutingGenerator.clusteredNodes(numCluster, nodesByCluster, r)
    val dist  = RoutingGenerator.distancesMatrix(nodes)

    nodes should have length numCluster * nodesByCluster

    for (n <- 0 until numCluster * nodesByCluster by nodesByCluster) {
      for (i <- n until n + nodesByCluster) {
        for (j <- i + 1 until n + nodesByCluster) {
          dist(i)(j) should be <= 2L * r.toLong
        }
      }
    }
  }

  test("Nodes are evenly space using evenlySpacedGenerator") {
    val nodesDist = 50L
    val numNodes  = 100
    RoutingGenerator.setSeed(Random.nextLong())
    println(s"Seed: ${RoutingGenerator.seed}")
    val depot = (42L, 42L)

    val nodes = RoutingGenerator.evenlySpacedNodes(numNodes, nodesDist, depot)

    nodes should have length numNodes
    // Nodes are generated from the depot and evenly spaced. So the modulo of their coordinates
    // must be the same than the modulo of the depot.
    for (n <- nodes) {
      n._1 % nodesDist should equal(depot._1 % nodesDist)
      n._2 % nodesDist should equal(depot._2 % nodesDist)
    }
  }

  test(
    "The expected number of nodes and the distance between them are too big for the " +
      "evenlySpacedGenerator"
  ) {
    val nodesDist = 500L
    val numNodes  = 100
    RoutingGenerator.setSeed(Random.nextLong())
    println(s"Seed: ${RoutingGenerator.seed}")
    val center = RoutingGenerator.centerDepot

    val nodes = RoutingGenerator.evenlySpacedNodes(numNodes, nodesDist, center)

    nodes should have length 8
    // Nodes are generated from the center and evenly spaced. So the modulo of their coordinate
    // must be the same than the modulo of the center.
    for (n <- nodes) {
      n._1 % nodesDist should equal(center._1 % nodesDist)
      n._2 % nodesDist should equal(center._2 % nodesDist)
    }
  }

}
