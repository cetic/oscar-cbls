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
import oscar.cp.core.CPStore;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Greater or Equal Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class GrEq extends Constraint {

	CPIntVar x, y;

    /**
     * Constraint x >= y
     * @param x
     * @param y
     * @see GrEqCteReif
     * @see GrEqVarReif
     */
	public GrEq(CPIntVar x, CPIntVar y) {
		super(x.store(),"GrEq");
		this.x = x;
		this.y = y;
	}
	
	public GrEq(CPIntVar x, int v) {
		this(x, CPIntVar.apply(v, v, x.store()));
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x, y));
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		priorityL2_$eq(CPStore.MaxPriorityL2());
		propagate();
		if(isActive()){
			if (!y.isBound()) y.callPropagateWhenBoundsChange(this);
			if (!x.isBound()) x.callPropagateWhenBoundsChange(this);
		}
	}
	
	@Override
	public void propagate() {
		if (x.getMin() >= y.getMax()) {
			deactivate();
			return;
		}
		x.updateMin(y.getMin());
		y.updateMax(x.getMax());
	}

}
