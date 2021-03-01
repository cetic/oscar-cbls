/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.examples.hakank

import oscar.cp._
import scala.io.Source._
import scala.math._
/*
  Scene Allocation problem in Oscar.
  From 
  Pascal Van Hentenryck
  "Constraint and Integer Programming in OPL"
  page 362 (or 18 depending how you count)
  """
  The scene-allocation problem was communicated to us by Irvin Lustig, 
  who also provided an integer-programming model and a specific instance. 
  It consists of deciding when to shoot scenes for a movie. Each scene 
  involves a number of actors and at most 5 scenes a day can be filmed. 
  All actors of a scene must, of course, be present on the day the scene 
  is shot. The actors have fees representing the amount to be paid per day 
  they spent in the studio. The goal of the application is to minimize 
  the production costs.
  """
  Also, see http://www.drdobbs.com/sticks/184404013
  by Dennis E. Shasha where the payments are * 10 compared to Van Hentenryck's
  http://collaboration.cmc.ec.gc.ca/science/rpn/biblio/ddj/Website/articles/DDJ/2000/0004/0004r/0004r.htm
  http://collaboration.cmc.ec.gc.ca/science/rpn/biblio/ddj/Website/articles/DDJ/2000/0001/0001n/0001n.htm
  http://cs.nyu.edu/shasha/papers/acks
  Answer:   
  - Van Hentenryck's payment: $334144
  - Shasha's payment: $3341440
  Thanks to Pierre Schaus for help with labeling and speedup.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object SceneAllocation extends CPModel with App {
  //
  // data
  //
  val maxDay = 5
  val numActors = 11
  val Array(patt, casta, scolaro, murphy, brown, hacket, anderson, mcdougal, mercer, spring, thompson) = (0 until numActors).toArray
  val actorS = Array("Patt", "Casta", "Scolaro", "Murphy", "Brown", "Hacket", "Anderson", "McDougal", "Mercer", "Spring", "Thompson")
  // Van Hentenryck's payment
  val pay = Array(26481, 25043, 30310, 4085, 7562, 9381, 8770, 5788, 7423, 3303, 9593)
  // Dennis E. Shasha's payments (10 * Van Hentenryck's payment)
  // val pay2 = pay.map(_*2)
  val appears = Array(Array(hacket), // 0
    Array(patt, hacket, brown, murphy), // 1
    Array(mcdougal, scolaro, mercer, brown), // 2
    Array(casta, mercer), // 3
    Array(mercer, anderson, patt, mcdougal, spring), // 4
    Array(thompson, mcdougal, anderson, scolaro, spring), // 5
    Array(casta, patt), // 6
    Array(mercer, murphy), // 7
    Array(casta, mcdougal, mercer, scolaro, thompson), // 8
    Array(casta, mcdougal, scolaro, patt), // 9
    Array(patt), // 10
    Array(hacket, thompson, mcdougal, murphy, brown), // 11
    Array(hacket, murphy, casta, patt), // 12
    Array(anderson, scolaro), // 13
    Array(thompson, murphy, mcdougal, patt), // 14
    Array(scolaro, mcdougal, casta, mercer), // 15
    Array(scolaro, patt, brown), // 16
    Array(scolaro, mcdougal, hacket, thompson), // 17
    Array(casta) // 18
  )
  val maxScene = appears.length
  val DAYS = 0 until maxDay
  val SCENES = 0 until maxScene
  val ACTORS = 0 until numActors
  // which scene does an actor appears in
  val which = for (a <- ACTORS) yield (for (
    s <- SCENES;
    a2 <- appears(s) if a == a2
  ) yield s);
  //
  // variables
  //
  val shoot = Array.fill(maxScene)(CPIntVar(0 until maxDay)) // the day a particular scene is shooted
  val cost = sum(ACTORS, DAYS)((a, d) => isOr(which(a))(shoot(_) ?=== d) * pay(a))
  //
  // constraints
  //
  var numSols = 0
  minimize(cost)
  add(gcc(shoot, 0 until maxDay, 0, 5), Strong)
  add(shoot(0) === 0)

  // This labeling is from the great mind of Pierre Schaus.
  // order the shoot by decreasing cost
  val orderedShoot = (0 until maxScene).sortBy(s => -appears(s).map(pay(_)).sum).map(shoot(_))
  search {
    if (allBounds(orderedShoot)) noAlternative
    else {
      val max = orderedShoot.filter(_.isBound).map(_.value).max
      val minsize = orderedShoot.filter(!_.isBound).map(_.size).min
      val x = orderedShoot.filter(_.size == minsize).head
      val xmin = x.min
      if (xmin == max + 1) {
        branchOne(post(x === xmin))
      } else {
        branch(post(x === xmin))(post(x !== xmin))
      }
    }
  }
  onSolution {
    println("\nSolution:")
    println("cost :" + cost)
    println("shoot:" + shoot.mkString(""))
    println()
    numSols += 1
  }
  println(start())
}
