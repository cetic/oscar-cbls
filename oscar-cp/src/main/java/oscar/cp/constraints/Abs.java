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
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Absolute value constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Abs extends Constraint {
	
	private CPIntVar x;
	private CPIntVar y;


    /**
     * Build a constraint y = |x|
     * @param x
     * @param y
     */
	public Abs(CPIntVar x, CPIntVar y) {
		super(x.store(),"Abs");
		this.x = x;
		this.y = y;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x, y));
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) {
		y.updateMin(0) ;
		propagate() ;
		if (!x.isBound()) {
			x.callPropagateWhenBoundsChange(this);
			x.callValBindWhenBind(this);
		}
		if (!y.isBound()) {
			y.callPropagateWhenBoundsChange(this);
			y.callValBindWhenBind(this);
		}
		//we can do more propagation with val remove
	}
	
	@Override
	public void propagate() {
		// y = |x|	
		
		if (x.getMin() >= 0) {
			y.updateMin(x.getMin());
			y.updateMax(x.getMax());
			x.updateMin(y.getMin());
			x.updateMax(y.getMax());
		}
		else if (x.getMax() <= 0) {
			y.updateMin(-x.getMax());
			y.updateMax(-x.getMin());
			x.updateMin(-y.getMax());
			x.updateMax(-y.getMin());
		} else {
			int maxabsy = Math.max(Math.abs(x.getMax()), Math.abs(x.getMin()));			
			y.updateMax(maxabsy);
			x.updateMax(y.getMax());
			x.updateMin(-y.getMax());
			
		}
	}
	
	@Override
	public void valBind(CPIntVar var) {
		//return Outcome.Suspend;
		
		if (x.isBound()) {
			y.assign(Math.abs(x.min()));
			deactivate();
		} else { // y is bound
			// y = |x|	
			if(!x.hasValue(-y.min())) {
				x.assign(y.min());
			}
			else if(!x.hasValue(y.min())) {
				x.assign(-y.min());
			}
			else {
				// x can be (y or -y)
				// remove everything except y and -y from x
				for (int v = x.getMin(); v <= x.getMax(); v++) {
					if(v != y.min() && v != -y.min()) {
						x.removeValue(v);
					}
				}
			}
			deactivate();
		}
	}
}
