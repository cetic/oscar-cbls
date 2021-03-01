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
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Sum Less Or Equal Constraint: x[0]+x[1]+...+x[n] <= y
 * @author Pierre Schaus pschaus@gmail.com
 */
public class SumLeEq extends Constraint {
	
	private CPIntVar [] x;
	private CPIntVar y;

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x));
		l.add(y);
		return CollectionConverters.asScala(l);
	}

	public SumLeEq(CPIntVar [] x, CPIntVar y) {
		super(x[0].store(),"SumLeq");
		this.x = x;
		this.y = y;
	}

    /**
     * sum(x) <= y
     * @param x
     * @param y
     */
	public SumLeEq(CPIntVar [] x, int y) {
		this(x,CPIntVar.apply(x[0].store(),y,y));
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		propagate();
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) 
				x[i].callPropagateWhenBoundsChange(this);
		}
		if (!y.isBound())
			y.callPropagateWhenBoundsChange(this);
	}
	
	@Override
	public void propagate() {
		int maxsumx = 0;
		int minsumx = 0;
		for (int i = 0; i < x.length; i++) {
			maxsumx += x[i].getMax();
			minsumx += x[i].getMin();
		}
		
		if (maxsumx <= y.getMin()) {
			deactivate();
			return;
		}
		
		y.updateMin(minsumx);
		
		for (int i = 0; i < x.length; i++) {
			int minsumxi = minsumx - x[i].getMin();
			int maxi = y.getMax() - minsumxi;
			x[i].updateMax(maxi);
		}
	}
	
	

}
