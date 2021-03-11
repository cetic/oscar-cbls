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
import oscar.cp.core.CPStore;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Reified Greater or Equal Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class LeEqCteReif extends Constraint {

	CPIntVar x;
	int v;
	CPBoolVar b;

    /**
     * Constraint x less or equal to v if and only if b is true <br>
     * x <= v <=> b
     * @param x
     * @param v
     * @param b
     */
	public LeEqCteReif(CPIntVar x, int v, CPBoolVar b) {
		super(x.store(),"GrEqCteReif");
		this.x = x;
		this.v = v;
		this.b = b;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x, b));
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		priorityBindL1_$eq(CPStore.MaxPriorityL1());
		priorityL2_$eq(CPStore.MaxPriorityL2()-1);
		propagate();
		if(isActive()){
			b.callValBindWhenBind(this);
			x.callPropagateWhenBoundsChange(this);
			if (b.isBound()) {
				valBind(b);
			}
		}
	}
	
	@Override
	public void propagate() {
		if (x.getMax() <= v) {
			b.assign(1);
			deactivate();
		} else if (x.getMin() > v) {
			b.assign(0);
			deactivate();
		}
	}
		
	@Override
	public void valBind(CPIntVar var) {
		if (b.min() == 0) {
			//x > v
			x.updateMin(v+1);
		} else {
			//x <= v
			x.updateMax(v);
		}
		deactivate();
	}

}

