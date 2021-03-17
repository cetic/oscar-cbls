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
import oscar.algo.reversible.ReversibleInt;
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
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Maximum extends Constraint {
	
	
	private CPIntVar [] x;
	private CPIntVar y;
	private ReversibleInt maxval;
	private ReversibleInt maxvalsupport;
	
	private ReversibleInt minval;
	private ReversibleInt minvalsupport;
	
	/**
	 * Constraint y = max(x)
	 * @param x
	 * @param y
	 */
	public Maximum(CPIntVar [] x, CPIntVar y) {
		super(x[0].store(),"Maximum");
		this.x = x;
		this.y = y;
		maxval = new ReversibleInt(s(), 0);
		maxvalsupport = new ReversibleInt(s(), 0);
		minval = new ReversibleInt(s(), 0);
		minvalsupport = new ReversibleInt(s(), 0);
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x));
		l.add(y);
		return CollectionConverters.asScala(l);
	}

	private void updateSupport() {
		int min = Integer.MIN_VALUE;
		int max = Integer.MIN_VALUE;
		for (int i = 0; i < x.length; i++) {
			int m = x[i].getMin();
			int M = x[i].getMax();
			
			if (m > min) {
				minvalsupport.setValue(i);
				minval.setValue(m);
				min = m;
			}
			if (M > max) {
				maxvalsupport.setValue(i);
				maxval.setValue(M);
				max = M;
			}
		}
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		
		for (int i=0; i < x.length; i++) {			
			x[i].updateMax(y.getMax());
		}
		updateSupport();
		y.updateMin(minval.getValue());
		y.updateMax(maxval.getValue());
		
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound() && (x[i].getMax() > y.getMin())) {
				x[i].callUpdateBoundsIdxWhenBoundsChange(this, i);
			}
		}
		if (!y.isBound()) {
			y.callUpdateBoundsWhenBoundsChange(this);
		}
	}
	
	@Override
	public void updateBoundsIdx(CPIntVar x, int idx) {
		if (idx == minvalsupport.getValue() || idx == maxvalsupport.getValue()) {
			updateSupport();
			y.updateMin(minval.getValue());
			y.updateMax(maxval.getValue());
		}
		if (x.isBound() && x.min() == maxval.getValue()) {
			y.assign(maxval.getValue());
			deactivate();
		}
	}
	
	
	@Override
	public void updateBounds(CPIntVar y) {
		for (int i=0; i < x.length; i++) {			
			x[i].updateMax(y.getMax());
		}
	}

}
