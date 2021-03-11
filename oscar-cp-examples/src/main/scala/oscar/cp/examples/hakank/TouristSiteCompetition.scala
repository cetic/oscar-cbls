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
  Tourist site competition in Oscar.
  From Pierre Flener's presentation 
  "Constraint Technology - A Programming Paradigm on the Rise"
  http://www.it.uu.se/edu/course/homepage/ai/vt08/AI-CT.pdf
     pages 5f: problem statement 
     pages 12f: model
     pages 21ff: walktrough of a solution
  With 7 tourist sites and 7 judges:
  """
  Every tourist site is visited by r = 3 judges.
  Every judge visits c = 3 tourist sites.
  Every pair of sites is visited by lambda = 1 common judge.
  """
  The same problem was also presented as "The Airline-of-the-Year Problem"
  in his (Flener's) presentation
  "Constraint Programming - Programming Paradigm on the Rise"
  http://www.it.uu.se/research/group/astra/ATM-CT/Flener.pdf
  page 4f
  The problem is stated as follows for 7 airlines and 7 judges:
  """
  Constant jury: Every airline is tested by 3 judges.
  Constant load: Every judge tests 3 airlines.
  Equity: Every airline pair is tested by 1 common judge.
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object TouristSiteCompetition extends CPModel with App  {
    //
    // data
    //
    val r = 3
    val c = 3
    val lambda = 1
    val num_sites = 7
    val sites = 0 until num_sites
    val Array(birka, falun, lund, mora, sigtuna, uppsala, ystad) = sites.toArray
    val sitesStr = Array("Birka", "Falun", "Lund", "Mora", "Sigtuna", "Uppsala", "Ystad")
    val num_judges = 7
    val judges = 0 until num_judges
    val Array(ali, dan, eva, jim, leo, mia, ulla) = judges.toArray
    val judgesStr = Array("Ali", "Dan", "Eva", "Jim", "Leo", "Mia", "Ulla")
    //
    // variables
    //
    val x = Array.fill(num_sites, num_judges)(CPBoolVar())
    val judges_where = Array.fill(num_judges, num_sites)(CPBoolVar())
    //
    // constraints
    //
    var numSols = 0
  
      //  Symmetry breaking
      for(s <- 0 until 3) {
       add(x(s)(0))
      }
      //  Every tourist site is visited by r judges.
      for(s <- sites) {
       add(sum(for(j <- judges) yield x(s)(j)) === r)
      }
      //  Every judge visits c tourist sites.
      for(j <- judges) {
       add(sum(for(s <- sites) yield x(s)(j)) === c)
      }
      //  Every pair of sites is visited by lambda common judge.
      for(s1 <- sites;
          s2 <- sites
            if s1 < s2) {
       add(sum(for(j <- judges) yield (x(s1)(j) && (x(s1)(j) ?=== x(s2)(j)))) === lambda)
      }
      //  where are the judges? (for presentation)
      for(j <- judges;
          s <- sites) {
       add((x(s)(j) ?=== 1) === (judges_where(j)(s) ?=== 1))
      }
    search{      
      binaryMaxDegree(x.flatten.toSeq)
    }
onSolution {
      println(x.map(i=>i.mkString(" ")).mkString("\n"))
      println("Judges:")
      println(judges.map(j=>judgesStr(j) + ": " + judges_where(j).
                         zipWithIndex.filter(_._1.value==1).map(_._2).
                         map(s=>sitesStr(s)).mkString(" ")).mkString("\n"))
      println()
      numSols += 1
    }
    println(start())
  }
