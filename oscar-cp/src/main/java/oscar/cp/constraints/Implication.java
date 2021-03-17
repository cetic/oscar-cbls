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
 * Reified Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Implication extends Constraint {

	CPIntVar A;
	CPIntVar B;
	CPIntVar V;

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(A, B, V));
		return CollectionConverters.asScala(l);
	}

    /**
     * Logical Implication: A => B <=> V
     * @param A
     * @param B
     * @param V
     */
	public Implication(CPBoolVar A, CPBoolVar B, CPBoolVar V) {
		super(A.store(),"Implication");
		this.A = A; 
		this.B = B;
		this.V = V;
	}
	
	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		if (A.isBound())
			valBind(A);
		else A.callValBindWhenBind(this);
		
		if (B.isBound())
			valBind(B);
		else B.callValBindWhenBind(this);
		
		if (V.isBound())
			valBind(V);
		else V.callValBindWhenBind(this);
	}

	
	
	protected int getPriorityBindL1(){
		return CPStore.MaxPriorityL1();
	}


	@Override
	public void valBind(CPIntVar var) {
		if (A.isBound()) {
			if (A.isBoundTo(0)) {
				// F => X is always true
				V.assign(1);
				this.deactivate();
				return;
			} else {
				// T => B <-> V
				if (B.isBoundTo(0)) { // T => F it means V must be F
					V.assign(0);
					this.deactivate();
					return;
				}
				if (B.isBoundTo(1)) { // T => T it means V must be T
					V.assign(1);
					this.deactivate();
					return;
				}
				// the case of whether V is bound is treated below
			}
		} 
		if (B.isBound()) {
			if (B.isBoundTo(1)) { // V is always true in this case
				V.assign(1);
				this.deactivate();
				return;
			} else {
				// A => F <-> V
				// case A is bound is treated above and V is bound is treated below
			}
		}
		if (V.isBound()) {
			if (V.min() == 0) {
				// only way to get A => B <-> F is to have T => F
				A.assign(1);
				B.assign(0);
				this.deactivate();
				return;
			} else {
				// V is True
				if (B.isBoundTo(0)) {  // A => F <-> T it means A must be F
					A.assign(0);
					this.deactivate();
					return;
				}
				if (B.isBoundTo(1)) { // A => T <-> T it means A can be true of false, doesn't matter
					this.deactivate();
					return;
				}
				if (A.isBoundTo(1)) {
					B.assign(1);
				}
			}
		}
	}
}

