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

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPBoolVar;
import oscar.cp.core.CPIntervalVar;
import oscar.cp.core.CPIntervalVar;
import oscar.cp.core.Constraint;

/**
 * Reified constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class EqReifIntervalVar extends Constraint {

	CPIntervalVar x;
	CPIntervalVar y;
	CPBoolVar b;
	

	/**
     * Ask that x and v take equals values if and only if b is true. <br>
     * (x == y) <=> b
     * @param x
     * @param y
     */
	public EqReifIntervalVar(CPIntervalVar x, CPIntervalVar y, CPBoolVar b) {
		super(x.store(),"EqReifIntervalVar");
		this.x = x;
		this.y = y;
		this.b = b;
	}
	
	@Override
	public CPOutcome setup(CPPropagStrength l) {
		if (b.isBound()) {
			return valBind(b);
		} 
		else if (x.isBound()) {
			return valBind(x);
		} 
		else if (y.isBound()) {
			return valBind(y);
		}
		else {
			x.callPropagateWhenBoundsChange(this,false);
			y.callPropagateWhenBoundsChange(this,false);
			b.callValBindWhenBind(this);
			x.callValBindWhenBind(this);
			y.callValBindWhenBind(this);
			return propagate();
		}
	}
	
	@Override
	public CPOutcome valBind(CPIntervalVar var) {
		if (b.isBound()) {
			deactivate();
			if (b.getValue() == 1) {
				// x == y
				if (s().post(new EqInterval(x,y)) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			} else {
				//x != y
				if (s().post(new DiffVarInterval(x,y))  == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			return CPOutcome.Success;
		}	
		else if (x.isBound()) {
			deactivate();
			if (s().post(new EqReifInterval(y,x.getValue(),b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else { // y.isBound()
			deactivate();
			if (s().post(new EqReifInterval(x,y.getValue(),b)) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
	}
	
	
	
	@Override
	public CPOutcome propagate() {
		// if the domains of x and y are disjoint we can set b to false and return success
		if (x.getMax() < y.getMin()) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else if (y.getMax() < x.getMin()) {
			if (b.assign(0) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
		}
		else {
			// there is an overlap between the domain ranges
			return CPOutcome.Suspend;
		}	
	}

}

