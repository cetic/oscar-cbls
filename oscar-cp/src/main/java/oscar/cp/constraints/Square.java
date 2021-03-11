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
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Square extends Constraint {

	private CPIntVar x;
	private CPIntVar y;

	/**
	 * x*x == y
	 * @param x
	 * @param y
	 * @see  CPIntVar#mul(cp.core.CPIntVar)
	 */
	public Square(CPIntVar x, CPIntVar y) {
		super(x.store(),"Square");
		this.x = x;
		this.y = y;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x, y));
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		y.updateMin(0);
		propagate();
		if(isActive()) {
			if (!x.isBound()) {
				x.callPropagateWhenBoundsChange(this);
			}
			if (!y.isBound()) {
				y.callPropagateWhenBoundsChange(this);
			}
		}
	}

	@Override
	public void propagate() {
		// propagation of y
		int mx = x.getMin();
		int Mx = x.getMax();
		int mx2 = mx*mx;
		int Mx2 = Mx*Mx;

		//propagate y (which is not bound)
		if (mx >= 0) { // x will be positive
			y.updateMin(mx2);
			y.updateMax(Mx2);
		} else if (Mx <= 0) { // x is non positive
			y.updateMin(Mx2);
			y.updateMax(mx2);
		} else if (x.hasValue(0)) {
			//y min is already >= 0 (post does it)
			y.updateMax(Math.max(mx2, Mx2));
		} else {
			Integer a = (Integer) x.valueBefore(0);
			Integer b = (Integer) x.valueAfter(0);
			int a2 = a*a;
			int b2 = b*b;
			y.updateMin(Math.min(a2, b2));
			y.updateMax(Math.max(a2, b2));
		}
		//propagate x (which is not bound)
		int my = y.getMin();
		int My = y.getMax();
		int my2 = my*my;
		int My2 = My*My;

		int rootm = (int) (Mx <= 0 ? Math.ceil(Math.sqrt(my)) : Math.sqrt(my));
		int rootM = (int) Math.sqrt(My);

		if (mx >= 0) {
			x.updateMin(rootm);
			x.updateMax(rootM);
		} else if (Mx <= 0) {
			x.updateMax(-rootm);
			x.updateMin(-rootM);
		} else {
			x.updateMin(-rootM);

			x.updateMax(rootM);
			/*
			for (int v = -rootm+1; v < rootm; v++) {
				x.removeValue(v);
			}*/
		}
	}

}
