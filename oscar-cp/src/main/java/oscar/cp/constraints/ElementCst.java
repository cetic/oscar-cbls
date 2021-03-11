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

import java.util.*;

import oscar.algo.reversible.ReversibleInt;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;


/**
 * Element Constraint on an array of constants
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ElementCst extends Constraint {
	

	private final int [] y;
	private CPIntVar x;
	private CPIntVar z;
	private Integer [] sortedPerm; //y[sortedPerm[0]] is the min element of y... y[sortedPerm[y.length]] is the largest element of y 
	private ReversibleInt minIndSupp;
	private ReversibleInt maxIndSupp;
	
	private Hashtable<Integer,ReversibleInt> counters;


    /**
     * @param y
     * @param x
     * @param z linked with y and x by the relation y[x] == z
     * @see  Element#get(int[], cp.core.CPIntVar)
     */
	public ElementCst(final int [] y, CPIntVar x, CPIntVar z) {
		super(x.store(),"ElementCst");
		this.y = y;
		this.x = x;
		this.z = z;
		
		sortedPerm = new Integer [y.length];
		for (int i = 0; i < y.length; i++) {
			sortedPerm[i] = i;	
		}
		Arrays.sort(sortedPerm, new Comparator<Integer>(){
	
			public int compare(Integer i1, Integer i2) {
				return (y[i1]-y[i2]);
			}});
		minIndSupp = new ReversibleInt(s(), 0);
		minIndSupp.setValue(0);
		maxIndSupp = new ReversibleInt(s(), 0);
		maxIndSupp.setValue(y.length-1);
		
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x, z));
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) {

		x.updateMin(0);
		x.updateMax(y.length-1);
		
		propagate();
		if (l == CPPropagStrength.Strong) {
			initCounters();
			x.callValRemoveWhenValueIsRemoved(this);
			z.callValRemoveWhenValueIsRemoved(this);
		}
		z.callPropagateWhenBoundsChange(this);
		x.callPropagateWhenDomainChanges(this);		
		x.callValBindWhenBind(this);
	}
	
	private void initCounters() {
		counters = new Hashtable<Integer, ReversibleInt>();
		for (int i = 0; i < y.length; i++) {
			ReversibleInt counter = counters.get(y[i]);
			if (counter == null) {
				counter = new ReversibleInt(s(),1);
				counters.put(y[i], counter);
			} else {
				counter.incr();
			}
		}
	}
	
	@Override
	public void valRemove(CPIntVar var, int val) {
		if (var == z) {
			for (int i = 0; i < y.length; i++) {
				if (y[i] == val) {
					x.removeValue(i);
				}
			}
		} else {
			assert(var == x);
			ReversibleInt counter = counters.get(y[val]);
			counter.decr();
			if (counter.getValue() == 0) {
				z.removeValue(y[val]);
			}
		}
	}

	@Override
	public void propagate() {
		// z = y[x] 
		int i = minIndSupp.getValue();
		while (i<y.length && (y[sortedPerm[i]] < z.getMin() || !x.hasValue(sortedPerm[i]))) {
			x.removeValue(sortedPerm[i]);
			i++;
		}
		minIndSupp.setValue(i);
		
		z.updateMin(y[sortedPerm[i]]);
		
		i = maxIndSupp.getValue();
		while (i>=0 && (y[sortedPerm[i]] > z.getMax() || !x.hasValue(sortedPerm[i]))) {
			x.removeValue(sortedPerm[i]);
			i--;
		}
		maxIndSupp.setValue(i);
		
		z.updateMax(y[sortedPerm[i]]);
	}
	@Override
	public void valBind(CPIntVar x) {
		// x is bound
		z.assign(y[x.min()]);
		deactivate();
	}

}
