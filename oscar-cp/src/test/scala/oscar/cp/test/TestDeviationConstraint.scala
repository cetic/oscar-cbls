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
package oscar.cp.test

import oscar.cp._
import oscar.cp.constraints._
import oscar.cp.testUtils.TestSuite


class TestDeviationConstraint extends TestSuite  {

  
    test ("testDeviation10") {
    	 val nbDevi = nbSol(false);
    	 val nbDecomp = nbSol(true);
         nbDevi should be(nbDecomp)
    }


    def nbSol(decomp: Boolean): Int = {
        val cp = CPSolver()
        val s = 74
    	val x = Array(CPIntVar(11 to 16)(cp), CPIntVar(9 to 11)(cp), CPIntVar(12 to 14)(cp), CPIntVar(13 to 14)(cp), CPIntVar(10 to 12)(cp), CPIntVar(12 to 15)(cp))
    	val nd = CPIntVar(0 to 34)(cp)

        if (decomp)
        	deviationDecomp(x,s,nd);
        else
        	cp.add(new Deviation(x, s, nd));

        var cnt = 0
        cp.search {
          binaryFirstFail(x)
        } 
        cp.start().nSols
    }

    def deviationDecomp(x: Array[CPIntVar], s: Int, nd: CPIntVar): Unit = {
        val cp = x(0).store;
        val dev: Array[CPIntVar] = Array.tabulate(x.length)(i => (mul(x(i),x.length)-s).abs)
		cp.post(new Sum(dev, nd))
        cp.post(new Sum(x, CPIntVar(s)(cp)))
    }
 
}
