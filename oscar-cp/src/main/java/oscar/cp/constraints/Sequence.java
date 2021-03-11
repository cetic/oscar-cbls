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
package oscar.cp.constraints;


import oscar.algo.Inconsistency;
import oscar.algo.reversible.SparseSet;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;


/**
 * Sequence constraint specifying that in any sequence of length q in x, there 
 * are at least min and most max occurrences from a value in set values. 
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Sequence extends Constraint {
	

	private CPIntVar[] xinit;
    private CPBoolVar[] x; // x[i] <-> (xinit[i] memberOf values)
	private int min, max, len;
    private SparseSet values;
    private CPIntVar[] cumulatedCounters; // cumulatedCounters[i] = x[0]+x[1]+...+x[i]
	private CPIntVar[][] P; // partial sums Pij = x[i]+...+x[j]
	
	/**
	 * Sequence constraint specifying that in any sequence of length q in x, there 
	 * are at least min and most max occurrences from a value in set values. 
	 * @param x the vector of variables constrained by the Sequence constraint. 
	 * @param values set of values which occurrence is counted within each sequence.
	 * @param l the length of the sequence
	 * @param min the minimal occurrences of values from set within a sequence.
	 * @param max the maximal occurrences of values from set within a sequence.
	 */
	public Sequence(CPIntVar [] x, SparseSet values, int l, int min, int max) {
		super(x[0].store(),"Sequence");
		assert (!(values.getSize() == 0));
		assert(l < x.length);
        assert(l > 0);
		assert(min <= max);
		assert(min > 0);
		assert(max <= l);
        this.xinit = x;
		this.values = values;
		this.len = l;
		this.min = min;
		this.max = max;
	}

    @Override
    public Iterable<CPVar> associatedVars() {
        List<CPVar> l = new LinkedList<>(Arrays.asList(x));
        return CollectionConverters.asScala(l);
    }

	@Override
	public void setup(CPPropagStrength cl) throws Inconsistency {
        // creates the bool vars and create the channeling constraints
        x = new CPBoolVar[xinit.length];
        for (int i = 0; i < x.length; i++) {
        	x[i] = CPBoolVar.apply(s());
        }
        for (int i = 0; i < x.length; i++) {
            s().post(new MemberReif(xinit[i],values,x[i]));
        }
        cumulatedCounters = new CPIntVar[x.length]; // cumulatedCounters[i] = x[0]+x[1]+...+x[i]
        cumulatedCounters[0] = x[0];
        for (int i = 1; i < x.length; i++) {
            cumulatedCounters[i] = oscar.cp.modeling.constraint.plus(cumulatedCounters[i-1],x[i]);
        }
        P = new CPIntVar[x.length][x.length];
        for (int i = 0; i < x.length; i++) {
            P[i][i] = x[i];
            for (int j = i+1; j < Math.min(x.length, i+len); j++) {
                if (i > 0)
                    P[i][j] = oscar.cp.modeling.constraint.minus(cumulatedCounters[j],cumulatedCounters[i-1]);
                else
                    P[i][j] = cumulatedCounters[j];
            }
        }
        for (int i = 0; i < x.length; i++) {
            for (int j = i+1; j < Math.min(x.length, i+len); j++) {
                for (int m = i; m < j; m++) {
                    s().post(new Sum(new CPIntVar[]{P[i][m],P[m+1][j]},P[i][j]));
                }
            }

            if (i <= x.length-len) {
               s().post(new GrEq(P[i][i+len-1],min));
               s().post(new LeEq(P[i][i+len-1],max));
            }

        }
	}
}
