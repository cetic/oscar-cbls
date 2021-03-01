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
 * Reified constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class DiffReif extends Constraint {

	CPIntVar x;
	int v;
	CPBoolVar b;
	

	/**
     * Ask that x and v take different values if and only if b is true. <br>
     * (x == v) <=> b
     * @param x
     * @param v
     */
	public DiffReif(CPIntVar x, int v, CPBoolVar b) {
		super(x.store(),"DiffReif");
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
	public void setup(CPPropagStrength l) {
		priorityBindL1_$eq(CPStore.MaxPriorityL1());
		priorityRemoveL1_$eq(CPStore.MaxPriorityL1());
		
		if (x.isBound() || b.isBound())
			valBind(x);
		else if (b.isBound())
			valBind(b);
		else {
			x.callValBindWhenBind(this);
			b.callValBindWhenBind(this);
			//x.addAC5Bounds(this);
			x.callValRemoveWhenValueIsRemoved(this);
		}
	}
	
	@Override
	public void updateBounds(CPIntVar x) {
		if (x.getMax() < v || x.getMin() > v) {
			b.assign(1);
			deactivate();
		}
	}
	

	@Override
	public void valRemove(CPIntVar x, int val) {
		if (val == v) {
			b.assign(1);
			deactivate();
		}
	}
	

	@Override
	public void valBind(CPIntVar var) {
		if (b.isBound()) {
			if (b.min() == 1) {
				//x != v
				x.removeValue(v);
			} else {
				//x == v
				x.assign(v);
			}
			deactivate();
		}
		else if (x.isBound()) {
			if (x.min() == v) {
				b.assign(0);
			}
			else {
				b.assign(1);
			}
			deactivate();
		}
	}

}

