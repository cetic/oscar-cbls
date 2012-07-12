/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Strictly Greater Than Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Gr extends Constraint {

	CPVarInt x, y;

    /**
     * x > y
     * @param x
     * @param y
     */
	public Gr(CPVarInt x, CPVarInt y) {
		super(x.getStore(),"Gr");
		this.x = x;
		this.y = y;
	}
	
	public Gr(CPVarInt x, int v) {
		this(x, new CPVarInt(x.getStore(),v,v));
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		CPOutcome oc = propagate();
		if(oc == CPOutcome.Suspend){
			if (!y.isBound()) y.callPropagateWhenMinChanges(this);
			if (!x.isBound()) x.callPropagateWhenMaxChanges(this);
		}
		return oc;
	}
	
	@Override
	protected CPOutcome propagate() {
		if (x.getMin() > y.getMax()) {
			return CPOutcome.Success;
		}
		if (x.updateMin(y.getMin()+1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (y.updateMax(x.getMax()-1) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Suspend;
	
	}

}