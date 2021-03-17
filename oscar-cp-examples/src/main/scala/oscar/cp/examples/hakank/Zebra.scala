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
  Note: A different (and simpler) approach is shown in Zebra2.scala.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object Zebra extends CPModel with App  {
    //
    // data
    //
    val n = 5
    //
    // variables
    // 
    // Colors
    val red           = CPIntVar(1 to n)
    val green         = CPIntVar(1 to n)
    val yellow        = CPIntVar(1 to n)
    val blue          = CPIntVar(1 to n)
    val ivory         = CPIntVar(1 to n)
    // Nationality
    val englishman    = CPIntVar(1 to n)
    val spaniard      = CPIntVar(1 to n)
    val japanese      = CPIntVar(1 to n)
    val ukrainian     = CPIntVar(1 to n)
    val norwegian     = CPIntVar(1 to n)
    // Animal
    val dog           = CPIntVar(1 to n)
    val snails        = CPIntVar(1 to n)
    val fox           = CPIntVar(1 to n)
    val zebra         = CPIntVar(1 to n)
    val horse         = CPIntVar(1 to n)
    // Drink
    val tea           = CPIntVar(1 to n)
    val coffee        = CPIntVar(1 to n)
    val water         = CPIntVar(1 to n)
    val milk          = CPIntVar(1 to n)
    val fruit_juice   = CPIntVar(1 to n)
    // Smoke
    val old_gold      = CPIntVar(1 to n)
    val kools         = CPIntVar(1 to n)
    val chesterfields = CPIntVar(1 to n)
    val lucky_strike  = CPIntVar(1 to n)
    val parliaments   = CPIntVar(1 to n)
    // for labeling
    val all_vars = Array(
                         parliaments, kools, chesterfields, lucky_strike, old_gold,
                         englishman, spaniard, japanese, ukrainian, norwegian,
                         dog, snails, fox, zebra, horse,
                         tea, coffee, water, milk, fruit_juice,
                         red, green, yellow, blue, ivory)
    //
    // constraints
    //
    var numSols = 0
  
      add(allDifferent(Array(red, green, yellow, blue, ivory)), Strong)
      add(allDifferent(Array(englishman, spaniard, japanese, ukrainian, norwegian)), Strong)
      add(allDifferent(Array(dog, snails, fox, zebra, horse)),Strong)
      add(allDifferent(Array(tea, coffee, water, milk, fruit_juice)), Strong)
      add(allDifferent(Array(parliaments, kools, chesterfields, lucky_strike, old_gold)), Strong)
       //
       // The clues
       //
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
      binaryStatic(all_vars)
    }
onSolution {
      println("\nSolution:")
      val p  = Array(englishman, spaniard, japanese, ukrainian, norwegian)
      val ps = Array("englishman", "spaniard", "japanese", "ukrainian", "norwegian")
      println("water drinker: " + ps((for{i <- 0 until n if p(i).value == water.value} yield i).head))
      println("owns zebra: " + ps((for{i <- 0 until n if p(i).value == zebra.value} yield i).head))
      numSols += 1
    } 
    println(start())
  }
