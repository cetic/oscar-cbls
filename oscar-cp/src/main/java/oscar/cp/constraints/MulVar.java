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
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import oscar.cp.util.ArrayUtils;
import oscar.cp.util.NumberUtils;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Multiplication Constraint x * y = z
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MulVar extends Constraint {

	private CPIntVar x, y, z;

    /**
     * x * y == z
     * @param x
     * @param y
     * @param z
     * @see CPIntVar#mul(cp.core.CPIntVar)
     */
	public MulVar(CPIntVar x, CPIntVar y, CPIntVar z) {
		super(x.store(),"Mul x*y=z");
		this.x = x;
		this.y = y;
		this.z = z;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x, y, z));
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		
		if (x == y) {
			s().post(new Square(x,z));
			deactivate();
			return;
		}
		if (z.isBound()) {
			if (z.min() == 0 && x.hasValue(0) && y.hasValue(0)) {
				x.callPropagateWhenDomainChanges(this);
				y.callPropagateWhenDomainChanges(this);
			} else {
				x.callPropagateWhenBoundsChange(this);
				y.callPropagateWhenBoundsChange(this);
			}
		} else {
			x.callPropagateWhenBoundsChange(this);
			y.callPropagateWhenBoundsChange(this);
			z.callPropagateWhenBoundsChange(this);
		}
		
		propagate();
	}
		
	@Override
	public void propagate() throws Inconsistency {
		if (!z.hasValue(0)) {
			x.removeValue(0);
			y.removeValue(0);
		}

		if (x.isBound()) { // y * c = z
			s().post(new MulCte(y,x.min(),z));
			deactivate();
		}
		else if (y.isBound()) { // x *c = z
			s().post(new MulCte(x,y.min(),z));
			deactivate();
		}
		else if (z.isBound()) { // x * y = c
			s().post(new MulCteRes(x,y,z.min()));
			deactivate();
		}
		else { // none of the variables are bound

			assert (!x.isBound() && !x.isBound() && !y.isBound());
			// propagation of z (try every combination of x and y's bounds)
			z.updateMin(ArrayUtils.min(NumberUtils.safeMul(x.getMin() , y.getMin()),
									   NumberUtils.safeMul(x.getMin() , y.getMax()),
									   NumberUtils.safeMul(x.getMax() , y.getMin()),
									   NumberUtils.safeMul(x.getMax() , y.getMax())));

			z.updateMax(ArrayUtils.max(NumberUtils.safeMul(x.getMin() , y.getMin()),
									   NumberUtils.safeMul(x.getMin() , y.getMax()),
									   NumberUtils.safeMul(x.getMax() , y.getMin()),
									   NumberUtils.safeMul(x.getMax() , y.getMax())));
			
			// propagate x
			propagateMul(x, y, z);
			// propagate y
			propagateMul(y, x, z);
		}
	}
	
	/**
	 * Set min(w) <-- min( ceil(a/c), ceil(a/d), ceil(b/c), ceil(b/d))
	 *     max(w) <-- max( floor(a/c), floor(a/d), floor(b/c), floor(b/d))  
	 * @param w
	 * @param a
	 * @param b
	 * @param c != 0
	 * @param d != 0
	 * @return Suspend if no failure detected during this propagation
	 */
	private void propagDiv(CPIntVar w, int a, int b, int c, int d) {
		int wmin = Math.min(NumberUtils.minCeilDiv(a, c, d), NumberUtils.minCeilDiv(b, c, d)); 
		w.updateMin(wmin);
		int wmax = Math.max(NumberUtils.maxFloorDiv(a, c, d), NumberUtils.maxFloorDiv(b, c, d)); 
		w.updateMax(wmax);
	}
	
	

	// propagate variable u for expression (u * w = z) with neither of the variable bound
	private void propagateMul(CPIntVar u, CPIntVar w, CPIntVar z) {
	   if (w.getMin() > 0 || w.getMax() < 0) {
		   propagDiv(u, z.getMin(), z.getMax(), w.getMin(), w.getMax());
		   return;
	   }
	   // w_min < 0 && w_max > 0.
	   else if (z.getMin() <= 0 && z.getMax() >= 0) {
		 // cannot filter u because we potentially have u * 0 = 0
	   }
	   else {
		 assert(!z.isBound());
		 int after0 = w.valueAfter(0);
		 int before0 = w.valueBefore(0);
		 if (w.getMin() == 0) {
			 propagDiv(u, z.getMin(), z.getMax(), after0, w.getMax());
		 }
		 else if (w.getMax() == 0) {
			 propagDiv(u, z.getMin(), z.getMax(), w.getMin(), before0);
		 }
		 else {
			 // w_min ... before0 ... 0 ... after0 ... w_max
			 int umin = Math.min(NumberUtils.minCeilDiv(z.getMin(), w.getMin(), w.getMax(), before0, after0),
								 NumberUtils.minCeilDiv(z.getMax(), w.getMin(), w.getMax(), before0, after0));
			 u.updateMin(umin);
			 int umax = Math.max(NumberUtils.maxFloorDiv(z.getMin(), w.getMin(), w.getMax(), before0, after0),
								 NumberUtils.minCeilDiv(z.getMax(), w.getMin(), w.getMax(), before0, after0));
			 u.updateMax(umax);
		 }
	  }
	}	
}



