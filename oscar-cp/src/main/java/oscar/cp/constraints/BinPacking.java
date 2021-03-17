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
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Bin-Packing constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class BinPacking extends Constraint {
	
	private CPIntVar [] x;
	private int [] w;
	private CPIntVar [] l;
	
	private CPBoolVar [][] b;

    /**
     * Links a number of weighted/sized items to be placed into a set of bins.
     * Available propagation strength are Weak and Strong (the default)
     * @param x, x[i] is the bin id in which item i is placed
     * @param w, w[i] is the size/weight of item i
     * @param l, l[j] is the load of bin j (sum of the weights of all items placed into bin j)
     * @see CPPropagStrength
     * @see BinaryKnapsack
     */
	public BinPacking(CPIntVar [] x, int [] w, CPIntVar [] l) {
		super(x[0].store(),"BinPacking");
		this.x = x;
		this.w = w;
		this.l = l;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x));
		l.addAll(Arrays.asList(this.l));
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength cl) throws Inconsistency {
		for (int i = 0; i < x.length; i++) {
			x[i].updateMin(0);
			x[i].updateMax(l.length-1);
		}
		
		b = new CPBoolVar[l.length][];
		//b[i][j] == true iff item x[j] is placed into bin i
		int totW = 0;
		for (int j = 0; j < x.length; j++) {
			totW += w[j];
		}
		for (int i = 0; i < b.length; i++) {
			b[i] = new CPBoolVar[x.length];
			for (int j = 0; j < x.length; j++) {
				b[i][j] = x[j].isEq(i);
			}
			s().post(new BinaryKnapsack(b[i],w,l[i]),cl);
		}
		for (int j = 0; j < x.length; j++) {
			CPBoolVar [] itemj = new CPBoolVar[l.length];
			for (int i = 0; i < itemj.length; i++) {
				itemj[i] = b[i][j];
			}
		}
		//redundant constraint
		s().post(new Sum(l,CPIntVar.apply(s(),totW,totW)));
	}
}
