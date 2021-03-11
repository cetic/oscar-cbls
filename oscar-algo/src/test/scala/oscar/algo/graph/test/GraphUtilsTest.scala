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
package oscar.algo.graph.test

import org.scalatest.{FunSuite, Matchers}
import oscar.algo.graph.{GraphUtils, NegativeWeightCycleException}

/**
 * Created on 06/03/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */

class GraphUtilsTest extends FunSuite with Matchers {

  test("Test of Dijkstra's shortest path algorithm for Int") {
    // The example was found at http://i.stack.imgur.com/YC8LA.gif
    val edgeCosts = Array(
      Array(0, 2, Int.MaxValue, 1, Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Array(Int.MaxValue, 0, Int.MaxValue, 3, 10, Int.MaxValue, Int.MaxValue),
      Array(4, Int.MaxValue, 0, Int.MaxValue, Int.MaxValue, 5, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, 2, 0, 2, 8, 4),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue, 6),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 1, 0)
    )

    val shortestPaths = GraphUtils.dijkstra(0, edgeCosts)
    shortestPaths(0)._1 shouldBe Array(0)
    shortestPaths(0)._2 shouldBe 0
    shortestPaths(1)._1 shouldBe Array(0, 1)
    shortestPaths(1)._2 shouldBe 2
    shortestPaths(2)._1 shouldBe Array(0, 3, 2)
    shortestPaths(2)._2 shouldBe 3
    shortestPaths(3)._1 shouldBe Array(0, 3)
    shortestPaths(3)._2 shouldBe 1
    shortestPaths(4)._1 shouldBe Array(0, 3, 4)
    shortestPaths(4)._2 shouldBe 3
    shortestPaths(5)._1 shouldBe Array(0, 3, 6, 5)
    shortestPaths(5)._2 shouldBe 6
    shortestPaths(6)._1 shouldBe Array(0, 3, 6)
    shortestPaths(6)._2 shouldBe 5
  }

  test("Test of Dijkstra's shortest path algorithm for Double") {
    // The example was found at http://www.math.cornell.edu/~numb3rs/blanco/net_dif.png
    val edgeCosts = Array(
      Array(0.0, Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.1, 0.9),
      Array(0.3, 0.0, 0.3, 0.4, Double.MaxValue, Double.MaxValue),
      Array(Double.MaxValue, Double.MaxValue, 0.0, 0.6, 0.4, Double.MaxValue),
      Array(Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.0, 1.0, Double.MaxValue),
      Array(0.55, Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.0, 0.45),
      Array(Double.MaxValue, Double.MaxValue, Double.MaxValue, 1.0, Double.MaxValue, 0.0)
    )

    val shortestPaths = GraphUtils.dijkstra(4, edgeCosts)
    shortestPaths(0)._1 shouldBe Array(4, 0)
    shortestPaths(0)._2 shouldBe 0.55
    shortestPaths(1)._1 shouldBe Array()
    shortestPaths(1)._2 shouldBe Double.MaxValue
    shortestPaths(2)._1 shouldBe Array()
    shortestPaths(2)._2 shouldBe Double.MaxValue
    shortestPaths(3)._1 shouldBe Array(4, 5, 3)
    shortestPaths(3)._2 shouldBe 1.45
    shortestPaths(4)._1 shouldBe Array(4)
    shortestPaths(4)._2 shouldBe 0.0
    shortestPaths(5)._1 shouldBe Array(4, 5)
    shortestPaths(5)._2 shouldBe 0.45
  }

  test("Test of Bellman-Ford-Moore's shortest path algorithm for Int") {
    // The example was found at http://i.stack.imgur.com/YC8LA.gif
    val edgeCosts = Array(
      Array(0, 2, Int.MaxValue, 1, Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Array(Int.MaxValue, 0, Int.MaxValue, 3, 10, Int.MaxValue, Int.MaxValue),
      Array(4, Int.MaxValue, 0, Int.MaxValue, Int.MaxValue, 5, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, 2, 0, 2, 8, 4),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue, 6),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 1, 0)
    )

    val shortestPaths = GraphUtils.bellmanFordMoore(0, edgeCosts)
    shortestPaths(0)._1 shouldBe Array(0)
    shortestPaths(0)._2 shouldBe 0
    shortestPaths(1)._1 shouldBe Array(0, 1)
    shortestPaths(1)._2 shouldBe 2
    shortestPaths(2)._1 shouldBe Array(0, 3, 2)
    shortestPaths(2)._2 shouldBe 3
    shortestPaths(3)._1 shouldBe Array(0, 3)
    shortestPaths(3)._2 shouldBe 1
    shortestPaths(4)._1 shouldBe Array(0, 3, 4)
    shortestPaths(4)._2 shouldBe 3
    shortestPaths(5)._1 shouldBe Array(0, 3, 6, 5)
    shortestPaths(5)._2 shouldBe 6
    shortestPaths(6)._1 shouldBe Array(0, 3, 6)
    shortestPaths(6)._2 shouldBe 5
  }

  test("Test of Bellman-Ford-Moore's shortest path algorithm for Double") {
    // The example was found at http://www.math.cornell.edu/~numb3rs/blanco/net_dif.png
    val edgeCosts = Array(
      Array(0.0, Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.1, 0.9),
      Array(0.3, 0.0, 0.3, 0.4, Double.MaxValue, Double.MaxValue),
      Array(Double.MaxValue, Double.MaxValue, 0.0, 0.6, 0.4, Double.MaxValue),
      Array(Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.0, 1.0, Double.MaxValue),
      Array(0.55, Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.0, 0.45),
      Array(Double.MaxValue, Double.MaxValue, Double.MaxValue, 1.0, Double.MaxValue, 0.0)
    )

    val shortestPaths = GraphUtils.bellmanFordMoore(4, edgeCosts)
    shortestPaths(0)._1 shouldBe Array(4, 0)
    shortestPaths(0)._2 shouldBe 0.55
    shortestPaths(1)._1 shouldBe Array()
    shortestPaths(1)._2 shouldBe Double.MaxValue
    shortestPaths(2)._1 shouldBe Array()
    shortestPaths(2)._2 shouldBe Double.MaxValue
    shortestPaths(3)._1 shouldBe Array(4, 5, 3)
    shortestPaths(3)._2 shouldBe 1.45
    shortestPaths(4)._1 shouldBe Array(4)
    shortestPaths(4)._2 shouldBe 0.0
    shortestPaths(5)._1 shouldBe Array(4, 5)
    shortestPaths(5)._2 shouldBe 0.45
  }

  test("Test of Bellman-Ford-Moore's shortest path algorithm with negative weights for Int") {
    // https://www.cpp.edu/~ftang/courses/CS241/notes/images/graph/bellman2.gif
    val edgeCosts = Array(
      Array(0, 5, Int.MaxValue, -2, Int.MaxValue, Int.MaxValue),
      Array(Int.MaxValue, 0, 1, Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, 0, Int.MaxValue, -1, 3),
      Array(Int.MaxValue, 2, 4, 0, 4, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, 1),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0)
    )

    val shortestPaths = GraphUtils.bellmanFordMoore(0, edgeCosts)
    shortestPaths(0)._1 shouldBe Array(0)
    shortestPaths(0)._2 shouldBe 0
    shortestPaths(1)._1 shouldBe Array(0, 3, 1)
    shortestPaths(1)._2 shouldBe 0
    shortestPaths(2)._1 shouldBe Array(0, 3, 1, 2)
    shortestPaths(2)._2 shouldBe 1
    shortestPaths(3)._1 shouldBe Array(0, 3)
    shortestPaths(3)._2 shouldBe -2
    shortestPaths(4)._1 shouldBe Array(0, 3, 1, 2, 4)
    shortestPaths(4)._2 shouldBe 0
    shortestPaths(5)._1 shouldBe Array(0, 3, 1, 2, 4, 5)
    shortestPaths(5)._2 shouldBe 1

    val shortestPaths2 = GraphUtils.bellmanFordMoore(4, edgeCosts)

    shortestPaths2(0)._1 shouldBe Array()
    shortestPaths2(0)._2 shouldBe Int.MaxValue
    shortestPaths2(1)._1 shouldBe Array()
    shortestPaths2(1)._2 shouldBe Int.MaxValue
    shortestPaths2(2)._1 shouldBe Array()
    shortestPaths2(2)._2 shouldBe Int.MaxValue
    shortestPaths2(3)._1 shouldBe Array()
    shortestPaths2(3)._2 shouldBe Int.MaxValue
    shortestPaths2(4)._1 shouldBe Array(4)
    shortestPaths2(4)._2 shouldBe 0
    shortestPaths2(5)._1 shouldBe Array(4, 5)
    shortestPaths2(5)._2 shouldBe 1
  }

  test("Test of Bellman-Ford-Moore's shortest path algorithm with negative weights for Double") {
    // https://www.cpp.edu/~ftang/courses/CS241/notes/images/graph/bellman2.gif
    val edgeCosts = Array(
      Array(0.0, 5.0, Double.MaxValue, -2.0, Double.MaxValue, Double.MaxValue),
      Array(Double.MaxValue, 0.0, 1.0, Double.MaxValue, Double.MaxValue, Double.MaxValue),
      Array(Double.MaxValue, Double.MaxValue, 0, Double.MaxValue, -1.0, 3.0),
      Array(Double.MaxValue, 2.0, 4.0, 0.0, 4.0, Double.MaxValue),
      Array(Double.MaxValue, Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.0, 1.0),
      Array(Double.MaxValue, Double.MaxValue, Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.0)
    )

    val shortestPaths = GraphUtils.bellmanFordMoore(0, edgeCosts)
    shortestPaths(0)._1 shouldBe Array(0)
    shortestPaths(0)._2 shouldBe 0
    shortestPaths(1)._1 shouldBe Array(0, 3, 1)
    shortestPaths(1)._2 shouldBe 0
    shortestPaths(2)._1 shouldBe Array(0, 3, 1, 2)
    shortestPaths(2)._2 shouldBe 1
    shortestPaths(3)._1 shouldBe Array(0, 3)
    shortestPaths(3)._2 shouldBe -2
    shortestPaths(4)._1 shouldBe Array(0, 3, 1, 2, 4)
    shortestPaths(4)._2 shouldBe 0
    shortestPaths(5)._1 shouldBe Array(0, 3, 1, 2, 4, 5)
    shortestPaths(5)._2 shouldBe 1

    val shortestPaths2 = GraphUtils.bellmanFordMoore(4, edgeCosts)

    shortestPaths2(0)._1 shouldBe Array()
    shortestPaths2(0)._2 shouldBe Double.MaxValue
    shortestPaths2(1)._1 shouldBe Array()
    shortestPaths2(1)._2 shouldBe Double.MaxValue
    shortestPaths2(2)._1 shouldBe Array()
    shortestPaths2(2)._2 shouldBe Double.MaxValue
    shortestPaths2(3)._1 shouldBe Array()
    shortestPaths2(3)._2 shouldBe Double.MaxValue
    shortestPaths2(4)._1 shouldBe Array(4)
    shortestPaths2(4)._2 shouldBe 0.0
    shortestPaths2(5)._1 shouldBe Array(4, 5)
    shortestPaths2(5)._2 shouldBe 1.0
  }

  test("Bellman-Ford-Moore's shortest path algorithm should detect negative-weight cycle for Int") {
    val edgeCosts = Array(
      Array(0, 5, Int.MaxValue, -2, Int.MaxValue, Int.MaxValue),
      Array(-3, 0, 1, Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, 0, Int.MaxValue, -1, 3),
      Array(Int.MaxValue, 2, 4, 0, 4, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, 1),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0)
    )

    a [NegativeWeightCycleException] should be thrownBy {
      GraphUtils.bellmanFordMoore(0, edgeCosts)
    }
  }

  test("Bellman-Ford-Moore's shortest path algorithm should detect negative-weight cycle for Double") {
    val edgeCosts = Array(
      Array(0.0, 5.0, Double.MaxValue, -2.0, Double.MaxValue, Double.MaxValue),
      Array(-3.0, 0.0, 1.0, Double.MaxValue, Double.MaxValue, Double.MaxValue),
      Array(Double.MaxValue, Double.MaxValue, 0.0, Double.MaxValue, -1.0, 3.0),
      Array(Double.MaxValue, 2.0, 4.0, 0.0, 4.0, Double.MaxValue),
      Array(Double.MaxValue, Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.0, 1.0),
      Array(Double.MaxValue, Double.MaxValue, Double.MaxValue, Double.MaxValue, Double.MaxValue, 0.0)
    )

    a [NegativeWeightCycleException] should be thrownBy {
      GraphUtils.bellmanFordMoore(0, edgeCosts)
    }
  }

}

