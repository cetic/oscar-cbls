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
import oscar.algo.reversible.ReversibleBoolean;
import oscar.algo.reversible.ReversibleInt;
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
 * Logical Or Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class OrReif2 extends Constraint {
	
	private CPBoolVar [] x;
	private CPBoolVar y;
	
	ReversibleInt nbBound;
	ReversibleBoolean ytrue;
	


    /**
     * y is true if at least one of the xi's is true, false otherwise
     * @param x
     * @param y
     */
	public OrReif2(CPBoolVar [] x, CPBoolVar y) {
		super(x[0].store(),"Or");
		this.x = x;
		this.y = y;

	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x));
		l.add(y);
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
	    if (x.length == 2) {
	        s().post(new BinaryOr(x[0],x[1],y));
			return;
	    }

		nbBound = new ReversibleInt(s(),0); // number of values assigned to false
		ytrue = new ReversibleBoolean(s(),false);
		for (int i = 0; i < x.length; i++) {
			if (x[i].isTrue()) {
				y.assign(1);
				return;
			}
		}

		// we know no variables from X are bound to 1
		for (int i = 0; i < x.length; i++) {
			if (!x[i].isBound()) {
				x[i].callValBindIdxWhenBind(this, i);
			} else {
				assert(x[i].isFalse());
				nbBound.incr();
			}
		}

		if (!y.isBound()) {
			if (nbBound.getValue() == x.length) {
				y.assign(0);
			}
			y.callValBindWhenBind(this);
		}
		else {
			if (y.min() == 0) {
				for (CPBoolVar aX : x) {
					aX.assign(0);
				}
				this.deactivate();
			} else { // y = true
				ytrue.setValue(true);
				if (nbBound.getValue() == x.length-1) { // only one is not bound to false, this one must be set to true
					for (int i = 0; i < x.length; i++) {
						if (!x[i].isBound()) {
							x[i].assign(1);
							this.deactivate();
							return;
						}
					}
				}
			}
		}
	}
	
	
	@Override
	public void valBindIdx(CPIntVar var, int idx) {
		if (var.min() == 1) {
			y.assign(1);
			this.deactivate();
			return;
		} else {
			nbBound.incr();			
			if (nbBound.getValue() == x.length) {
				y.assign(0);
			} else if (nbBound.getValue() == x.length-1 && ytrue.getValue()){
				for (int i = 0; i < x.length; i++) {
					if (!x[i].isBound()) {
						x[i].assign(1);
						this.deactivate();
						return;
					}
				}
			}
		}
	}
	
	@Override
	public void valBind(CPIntVar yvar) {
		if (yvar.min() == 0) {
			for (int i = 0; i < x.length; i++)
				x[i].assign(0);
			this.deactivate();
		} else {
			ytrue.setValue(true);
			if (nbBound.getValue() == x.length-1){
				for (int i = 0; i < x.length; i++) {
					if (!x[i].isBound()) {
						x[i].assign(1);
						this.deactivate();
						return;
					}
				}
			}
		}
	}

}
