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
 * Reified constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class DiffReifVar extends Constraint {

	CPIntVar x;
	CPIntVar y;
	CPBoolVar b;
	

	/**
     * Ask that x and v take different values if and only if b is true. <br>
     * (x != y) <=> b
     * @param x
     * @param y
     */
	public DiffReifVar(CPIntVar x, CPIntVar y, CPBoolVar b) {
		super(x.store(),"DiffReif");
		this.x = x;
		this.y = y;
		this.b = b;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x, y, b));
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		if (b.isBound()) {
			valBind(b);
		} 
		else if (x.isBound()) {
			valBind(x);
		} 
		else if (y.isBound()) {
			valBind(y);
		}
		else {
			x.callPropagateWhenDomainChanges(this);
			y.callPropagateWhenDomainChanges(this);	
			b.callValBindWhenBind(this);
			x.callValBindWhenBind(this);
			y.callValBindWhenBind(this);
			propagate();
		}
	}
	
	@Override
	public void valBind(CPIntVar var) throws Inconsistency {
		if (b.isBound()) {
			if (b.min() == 1) {
				// x != y
				s().post(new DiffVar(x,y));
			} else {
				//x == y
				s().post(new Eq(x,y));
			}
		}	
		else if (x.isBound()) {
			s().post(new DiffReif(y,x.min(),b));
		}
		else if (y.isBound()) {
			s().post(new DiffReif(x,y.min(),b));
		}
		deactivate();
	}
	
	
	
	@Override
	public void propagate() {
		// if the domains of x and y are disjoint we can set b to false and return success
		if (x.getMax() < x.getMin()) {
			b.assign(1);
			deactivate();
		}
		else if (y.getMax() < x.getMin()) {
			b.assign(1);
			deactivate();
		}
		else {
			// there is an overlap between the domain ranges
			// if no values in this overlapping range are common, set b to false
			int start = Math.max(x.getMin(), y.getMin());
			int end = Math.min(x.getMax(), y.getMax());
			boolean commonValues = false;
			for (int i = start; i <= end; i++) {
 				if (x.hasValue(i) && y.hasValue(i)) {
					commonValues = true;
					break;
				}
			}
			if (!commonValues) {
				b.assign(1);
				deactivate();
			}
		}
	}
	


}

