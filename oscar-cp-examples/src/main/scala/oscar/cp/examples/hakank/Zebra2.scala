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
package oscar.cp.examples.hakank

import oscar.cp._
import scala.io.Source._
import scala.math._
/*
  Zebra problem in Oscar.
  """
  This is the zebra problem as invented by Lewis Caroll.
  There are five houses.
  The Englishman lives in the red house.
  The Spaniard owns the dog.
  Coffee is drunk in the green house.
  The Ukrainian drinks tea.
  The green house is immediately to the right of the ivory house.
  The Old Gold smoker owns snails.
  Kools are smoked in the yellow house.
  Milk is drunk in the middle house.
  The Norwegian lives in the first house.
  The man who smokes Chesterfields lives in the house next to the man
    with the fox.
  Kools are smoked in the house next to the house where the horse is kept.
  The Lucky Strike smoker drinks orange juice.
  The Japanese smokes Parliaments.
  The Norwegian lives next to the blue house.
  Who owns a zebra and who drinks water?
  """
  This is a slightly alternative version of 
     http://www.hakank.org/oscar/Zebra.scala
  The difference is mostly how the decision variables are declared
  which makes it a little neater model.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object Zebra2 extends CPModel with App {
    //
    // data
    //
    val n = 5
    //
    // variables
    // 
    val colors      = Array.fill(n)(CPIntVar(1 to n))
    val Array(red, green, yellow, blue, ivory) = colors
    val nationality = Array.fill(n)(CPIntVar(1 to n))
    val Array(englishman,spaniard,japanese,ukrainian,norwegian) = nationality
    val animal      = Array.fill(n)(CPIntVar(1 to n))
    val Array(dog,snails,fox,zebra,horse) = animal
    val drink       = Array.fill(n)(CPIntVar(1 to n))
    val Array(tea,coffee,water,milk,fruit_juice) = drink
    val smoke       = Array.fill(n)(CPIntVar(1 to n))
    val Array(old_gold,kools,chesterfields,lucky_strike,parliaments) = smoke
    // for labeling
    val all_vars = colors ++ nationality ++ animal ++ drink ++ smoke
    //
    // constraints
    //
    var numSols = 0
    
      add(allDifferent(colors), Strong)
      add(allDifferent(nationality), Strong)
      add(allDifferent(animal),Strong)
      add(allDifferent(drink), Strong)
      add(allDifferent(smoke), Strong)
       // The clues
      add(englishman === red)
      add(spaniard === dog)
      add(coffee === green)
      add(ukrainian === tea)
      add(green === ivory + 1)
      add(old_gold === snails)
      add(kools === yellow)
      add(milk === 3)
      add(norwegian === 1)
      add((fox - chesterfields).abs === 1)
      add((horse - kools).abs === 1)
      add(lucky_strike === fruit_juice)
      add(japanese === parliaments)
      add((norwegian - blue).abs === 1)
    search{       
      binaryFirstFail(all_vars)
    }
onSolution {
      println("\nSolution:")
      val ns = Array("englishman", "spaniard", "japanese", "ukrainian", "norwegian")
      println("water drinker: " + 
              ns((for{i <- 0 until n if nationality(i).value == water.value} yield i).head))
      println("owns zebra: " + 
              ns((for{i <- 0 until n if nationality(i).value == zebra.value} yield i).head))
      numSols += 1
    } 
    println(start())
  }
