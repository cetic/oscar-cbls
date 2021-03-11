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
/*
  Exodus puzzle (Dell Logic Puzzles) in Oscar.
  From 
  http://brownbuffalo.sourceforge.net/ExodusClues.html
  """
  Title: Exodus
  Author: Sophy McHannot
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 14
  Stars: 2
  In preparation for Passover, five children at Hebrew school 
  (Bernice,Carl,Debby,Sammy, and Ted) 
  have been chosen to present
  different parts of the story of the Exodus from Egypt 
   (burning bush, captivity,
    Moses's youth, Passover, or the Ten Commandments). 
  Each child is a different age 
    (three, five, seven, eight, or ten), 
  and the family of each child has recently made its own exodus 
  to America from a different country 
  (Ethiopia, Kazakhstan, Lithuania, Morocco, or Yemen). 
  Can you find the age of each child, his or her family's country of 
  origin, and the part of the Exodus story each related?
   1. Debby's family is from Lithuania.
   2. The child who told the story of the Passover is two years older
      than Bernice.
   3. The child whose family is from Yemen is younger than the child from
      the Ethiopian family.
   4. The child from the Moroccan family is three years older than Ted.
   5. Sammy is three years older than the child who told the story of
      Moses's youth in the house of the Pharaoh.
   6. Carl related the story of the captivity of the Israelites in Egypt.
   7. The five-year-old child told the story of the Ten Commandments.
   8. The child who told the story of the burning bush is either two or
      three years older than the one whose family came from
      Kazakhstan.
  Determine: Age -- Child -- Country -- Story
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object Exodus extends CPModel with App {
  // 
  // Decomposition of inverse constraint
  // 
  // Channel of positions of x and y:
  //    j == x(i) <=> y(j) == i
  // 
  // Note: This requires the domain 0..n-1
  //
  def inverse(x: Array[CPIntVar], y: Array[CPIntVar]): Unit = {
    val len = x.length
    for (
      i <- 0 until len;
      j <- 0 until len
    ) {
      add((y(j) ?=== i) === (x(i) ?=== j))
    }
  }
  // Convenient function which returns y (for presentation)
  def inverse2(x: Array[CPIntVar]): Array[CPIntVar] = {
    val y = Array.fill(x.length)(CPIntVar(x(0).min to x(0).max))
    inverse(x, y)
    y
  }
  //
  // data
  //
  val n = 5
  val Array(bernice, carl, debby, sammy, ted) = (0 to n - 1).toArray
  val names = Array("Bernice", "Carl", "Debby", "Sammy", "Ted")
  //
  // variables
  //
  val story = Array.fill(n)(CPIntVar(0 to n - 1))
  val Array(burningbush, captivity, mosessyouth, passover, tencommandments) = story
  // for output
  val storyStr = Array("Burning Bush", "Captivity", "Moses Youth", "Passover", "Ten Commandments")
  val storyInv = inverse2(story)
  val country = Array.fill(n)(CPIntVar(0 to n - 1))
  val Array(ethiopia, kazakhstan, lithuania, morocco, yemen) = country
  // for output
  val countryStr = Array("Ethiopia", "Kazakhstan", "Lithuania", "Morocco", "Yemen")
  val countryInv = inverse2(country)
  val age = Array.fill(n)(CPIntVar(Array(3, 5, 7, 8, 10)))
  //
  // constraints
  //
  var numSols = 0

  add(allDifferent(story), Strong)
  add(allDifferent(country), Strong)
  add(allDifferent(age), Strong)
  add(lithuania === debby)
  add(age(passover) === age(bernice) + 2)
  add(age(yemen) < age(ethiopia))
  add(age(morocco) === age(ted) + 3)
  add(age(sammy) === age(mosessyouth) + 3)
  add(captivity === carl)
  add(age(tencommandments) === 5)
  add(
    (age(burningbush) ?=== age(kazakhstan) + 2)
      ||
      (age(burningbush) ?=== age(kazakhstan) + 3)
  )
  search {
    binaryStatic(story ++ country ++ age)
  }
  onSolution {
    println("Names  : " + names.mkString(" "))
    println("Story  :" + story.mkString(""))
    println("Country:" + country.mkString(""))
    println("Age    :" + age.mkString(""))
    println()
    println("Names  : " + names.mkString(", "))
    println("Story  : " + storyInv.map(s => storyStr(s.value)).mkString(", "))
    println("Country: " + countryInv.map(s => countryStr(s.value)).mkString(", "))
    println("Age    : " + age.mkString(", "))
    println()
    println((0 until n).
      map(s => Array(names(s), age(s), storyStr(storyInv(s).value), countryStr(countryInv(s).value)).mkString(", ")).mkString("\n"))
    println()
    numSols += 1
  }
  println(start())
}
