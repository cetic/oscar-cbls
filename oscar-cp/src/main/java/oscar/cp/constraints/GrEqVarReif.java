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
public class GrEqVarReif extends Constraint {

	CPIntVar x, y;
	CPBoolVar b;

    /**
     * Constraint x greater or equal to y if and only if b is true <br>
     * x >= y <=> b
     * @param x
     * @param y
     * @param b
     */
	public GrEqVarReif(CPIntVar x, CPIntVar y, CPBoolVar b) {
		super(x.store(),"GrEqVarReif");
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
		if (x.isBound()) {
			s().post(new LeEqCteReif(y, x.min(), b));
			deactivate();
			return;
		} else if (y.isBound()) {
			s().post(new GrEqCteReif(x, y.min(), b));
			deactivate();
			return;
		}
		
		propagate();
		if (isActive()){
			if (!b.isBound()) b.callValBindWhenBind(this);
			if (!x.isBound()) x.callPropagateWhenBoundsChange(this);
			if (!y.isBound()) y.callPropagateWhenBoundsChange(this);
			if (b.isBound()) {
				valBind(b);
			}
		}
	}
	
	@Override
	public void propagate() {
		if (x.getMin() >= y.getMax()) {
			b.assign(1);
			deactivate();
		}
		else if (x.getMax() < y.getMin()) {
			b.assign(0);
			deactivate();
		}
	}
	
	
	protected int getPriorityBindL1(){
		return CPStore.MaxPriorityL1()-1;
	}
		
	@Override
	public void valBind(CPIntVar var) throws Inconsistency {
		if (b.min() == 0) {
			//x < y
			s().post(new Le(x,y));
		} else {
			//x >= v
			s().post(new GrEq(x,y));
		}
		deactivate();
	}

}

