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

import oscar.algo.reversible.ReversibleBoolean;
import oscar.algo.reversible.ReversibleInt;
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
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AtLeastNValueFWC extends Constraint {
	
	private CPIntVar [] x;
	
	private CPIntVar nValueVar;
	
	private ReversibleBoolean [] isValueUsed; //for each value if it is used or not
	private ReversibleInt nbValueUsed; //number of value used
	private ReversibleInt nbBound; //number of bound variables


	private int min;
	private int max;
	private int valSize;
	
	
	public AtLeastNValueFWC(CPIntVar [] x, CPIntVar nval) {
		super(x[0].store(),"AtLeastNValueFWC");
		this.x = x;
		this.nValueVar = nval;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x));
		l.add(nValueVar);
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) {
	    
	     findValueRange();

	     //initialize trails and counters
	     isValueUsed   = new ReversibleBoolean[valSize];
	     for (int v = 0; v < valSize; v++) {
	    	 isValueUsed[v] = new ReversibleBoolean(s());
	    	 isValueUsed[v].setValue(false);
	     }
	     nbValueUsed = new ReversibleInt(s(), 0);
	     nbValueUsed.setValue(0);
	     nbBound = new ReversibleInt(s(), 0);
	     nbBound.setValue(0);
	     	    
	     for (int k = 0; k < x.length; k++) {
	       if (x[k].isBound()) {
	    	 int v = x[k].min();
	         nbBound.incr();
	         if (!isValueUsed[v-min].getValue()) {
	           nbValueUsed.incr();
	           isValueUsed[v-min].setValue(true);
	         }
	       }
	     }

	     //update lower bound on the number of values
	     nValueVar.updateMin(Math.max(nbValueUsed.getValue(), x.length>0 ? 1:0));

	     //update upper bound on the number of values
	     nValueVar.updateMax(nbValueUsed.getValue()+x.length-nbBound.getValue());

	     for (int k=0; k < x.length; k++) {
	       if (!x[k].isBound())
	         x[k].callValBindIdxWhenBind(this,k);
	       	 x[k].callPropagateWhenBind(this);
	     }
	     if (!nValueVar.isBound()) {
	       nValueVar.callPropagateWhenBoundsChange(this);
	     }

	     int ubNbValueUsed = nbValueUsed.getValue() + (x.length -nbBound.getValue());
	     if(ubNbValueUsed <= nValueVar.getMin()){
	       prune();
	     }
	}
	
	@Override
	public void valBindIdx(CPIntVar var, int idx) {
		
		int val = var.min();
		nbBound.incr();
		if(!isValueUsed[val-min].getValue()){
			nbValueUsed.incr();
			isValueUsed[val-min].setValue(true);
		}

		int ubNbValueUsed = nbValueUsed.getValue() + (x.length-nbBound.getValue());

		nValueVar.updateMin(nbValueUsed.getValue());
		nValueVar.updateMax(ubNbValueUsed);

		if(ubNbValueUsed == nValueVar.getMin()){
			prune();
		}
	}
	
	@Override
	public void propagate() {
		//_nValueVar has changed
		int ubNbValueUsed = nbValueUsed.getValue() + (x.length - nbBound.getValue());
		if (ubNbValueUsed == nValueVar.getMin()) {
			prune();
		}
	}
	
	public void prune(){
	  //remove used values from unbound variables
	  int [] values = new int[x.length];
	  int nb = 0;
	  for (int k = 0; k < x.length; k++) {
		if (x[k].isBound()) {
		  values[nb] = x[k].min();
		  nb++;
		}
	  }
	  for (int k = 0; k < x.length; k++) {
		if (!x[k].isBound()) {
		  for (int i = 0; i < nb; i++) {
			x[k].removeValue(values[i]);
		  }
		}
	  }
	}
	
	private void findValueRange(){
		min = Integer.MAX_VALUE;
		max = Integer.MIN_VALUE;
		for(int i = 0; i < x.length; i++) {
			min = Math.min(min, x[i].getMin());
			max = Math.max(max, x[i].getMax());
		}
		valSize = max - min + 1;
	}

}
