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
import oscar.algo.reversible.ReversibleInt;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Lexicographic Less or Equal (LexLeq) Constraint
 * DX TX refer to states name from the paper of M. Carlson & N Beldiceanu
 * and variable names as well.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class LexLeq extends Constraint {
	   
	private CPIntVar [] x;
	private CPIntVar [] y;
	
	private ReversibleInt q;
	private ReversibleInt r;
	private ReversibleInt s;
	private ReversibleInt u; // current state of the automaton
	
	private int i;
	
	private boolean posted;

    /**
     * Constraints the vector x to be lexicographically less or equal to y (lexleq). <br>
     * x lexleq y iff x[i] < y[i] or x[i] == y[i] and x[i+1,...,n] lexleq y[i+1,...,n] <br>
     * example: [0,0,1] lexleq [0,1,0]
     * @param x
     * @param y a vector of same length as x
     */
	public LexLeq(CPIntVar [] x, CPIntVar [] y) {
		super(x[0].store(),"LexLeq");
			
		if (x.length != y.length) {
			throw new RuntimeException("LexLeq: x and y must have the same length");
		}
		
		this.x = x;
		this.y = y;
		q = new ReversibleInt(super.s(), 0);
		r = new ReversibleInt(super.s(), 0);		
		s = new ReversibleInt(super.s(), 0);
		u = new ReversibleInt(super.s(), 0);
		u.setValue(0);
		
		posted = false;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x));
		l.addAll(Arrays.asList(y));
		return CollectionConverters.asScala(l);
	}

	public void setup(CPPropagStrength l) throws Inconsistency {
		  mySetup(l);
		  posted = true;
	}
	
	
	
	
	private void mySetup(CPPropagStrength l) throws Inconsistency {
	   i = 0;
	   q.setValue(0);
	   r.setValue(0);
	   s.setValue(0);
	   state1();
	}
	
	private void setupFrom(int p) {
		if (posted) return;
		for (int i = p ;i < x.length; i++) {
			if (!x[i].isBound()) {
				x[i].callUpdateBoundsIdxWhenBoundsChange(this, i);
			}
			if (!y[i].isBound()) {
				y[i].callUpdateBoundsIdxWhenBoundsChange(this, i);
			}
		}
	}
	
	@Override
	public void updateBoundsIdx(CPIntVar var, int idx) throws Inconsistency {
		i = idx;
		if (i == q.getValue()) state1();
		else if (i == r.getValue()) state2();
		else if (u.getValue() == 3 && (i == s.getValue() || (i < s.getValue() && x[i].getMax() != y[i].getMin()))) state3();
		else if (u.getValue() == 4 && (i == s.getValue() || (i < s.getValue() && x[i].getMin() != y[i].getMax()))) state4();
	}
	
  
	private void state1() throws Inconsistency {
		while(i < x.length && x[i].getMin() == y[i].getMax()) {
			int val = x[i].getMin();
			x[i].assign(val);
			y[i].assign(val);
			q.setValue(i = i + 1);
		}
		if (i >= x.length || x[i].getMax() < y[i].getMin()) {
			if (posted) deactivate();
			return;
		}
		x[i].updateMax(y[i].getMax());
		y[i].updateMin(x[i].getMin());

		i = i+1 > r.getValue() ? i+1 : r.getValue();
		r.setValue(i);

		state2();
	}
	   
	private void state2() throws Inconsistency {
		while(i < x.length && x[i].isBound() && y[i].isBound() && x[i].getMin() == y[i].getMin())  //STATE 2
			r.setValue(i = i + 1);

		if (i >= x.length || x[i].getMax() < y[i].getMin()) {
			if (posted) deactivate(); // deactivate the constraint since it is now replaced by a new one
			super.s().post(new LeEq(x[q.getValue()],y[q.getValue()])); // T3
			return;
		}

		if (x[i].getMin() > y[i].getMax()) {
			if (posted) deactivate();
			super.s().post(new Le(x[q.getValue()],y[q.getValue()])); // T2
			return;
		}

		if (x[i].getMax() == y[i].getMin() && x[i].getMin() < y[i].getMax()) {
			i = i+1 > s.getValue() ? i+1 : s.getValue();
			s.setValue(i);
			state3();
			return;
		}

		if (x[i].getMin() == y[i].getMax() && x[i].getMax() > y[i].getMin()) {
			i = i+1 > s.getValue() ? i+1 : s.getValue();
			s.setValue(i);
			state4();
			return;
		}
		setupFrom(q.getValue());
		u.setValue(2);
	}
	
	private void state3() throws Inconsistency {
		while(i < x.length && x[i].getMax() == y[i].getMin())
			i = i+1;
		s.setValue(i);
		if (i>= x.length || x[i].getMax() < y[i].getMin()) {
			if (posted) super.deactivate();
			super.s().post(new LeEq(x[q.getValue()], y[q.getValue()])); // T3
			return;
		}
		setupFrom(q.getValue());
		u.setValue(3);
	}
	
	private void state4() throws Inconsistency {
		while (i < x.length && x[i].getMin() == y[i].getMax())
			i = i + 1;
		s.setValue(i);
		if (i < x.length && x[i].getMin() > y[i].getMax()) {
			if (posted) super.deactivate();
			super.s().post(new Le(x[q.getValue()], y[q.getValue()]));
			return;
		}
		setupFrom(q.getValue());
		u.setValue(4);
	}
}
