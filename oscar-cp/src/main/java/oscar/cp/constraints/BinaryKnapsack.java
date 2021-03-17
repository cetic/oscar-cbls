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

import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import oscar.algo.Inconsistency;
import oscar.algo.reversible.ReversibleInt;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.CPStore;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;


/**
 * Binary Knapsack Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class BinaryKnapsack extends Constraint {
	
	CPBoolVar [] x;
	int [] w;
	CPIntVar c;

	ReversibleInt []  candidate;//index of items 0-1 (we dont't know if they are packed)
	ReversibleInt   rcap;     //required cap: sum of weight of required items with x=1 (packed for sure in the knapsack)
	ReversibleInt   pcap;     //possible cap: sum of weight of of possible items (candidate + the ones already packed)
	ReversibleInt   nb;       //number of possible items
	
	int alpha_;
	int beta_;
	int [] X;
    int n = -1; //number of element in the knapsack

    public BinaryKnapsack(CPBoolVar [] b, final int [] weights, CPIntVar load, int n) {
        this(b,weights,load);
        this.n = n;
        assert (n > 0);
    }

    public BinaryKnapsack(CPBoolVar [] b, final int [] weights, int load, int n) {
        this(b,weights,load);
        this.n = n;
        assert (n > 0);
    }

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x));
		l.add(c);
		return CollectionConverters.asScala(l);
	}

    /**
     * Constraint: load is the sum of the weights of items selected in to the knapsack. <br>
     * load = sum_i b[i]*weights[i] <br>
     * Available propagation strength are Weak and Strong (the default)
     * @param b
     * @param weights a vector of non negative integers of the same length as b
     * @param load
     * @see  CPPropagStrength
     */
	public BinaryKnapsack(CPBoolVar [] b, final int [] weights, CPIntVar load) {
		super(b[0].store(),"BinaryKnapsack");
        assert(b.length == weights.length);
		priorityL2_$eq(CPStore.MaxPriorityL2()-2);
		Integer [] perm = new Integer [weights.length];
		for (int i = 0; i < perm.length; i++) {
            assert (weights[i] >= 0);
			perm[i] = i;
		}
		
		Arrays.sort(perm, new Comparator<Integer>(){

			public int compare(Integer o1, Integer o2) {
				return weights[o2]-weights[o1];
			}
		});
		
		w = new int[weights.length];
		x = new CPBoolVar[weights.length];
		c = load;
		for (int i = 0; i < x.length; i++) {
			w[i] = weights[perm[i]];
			x[i] = b[perm[i]];
		}		
	}

    /**
     * Constraint: load is the sum of the weights of items selected in to the knapsack. <br>
     * load = sum_i b[i]*weights[i] <br>
     * Available propagation strength are Weak and Strong (the default)
     * @param b
     * @param weights a vector of non negative integers of the same length as b
     * @param load
     * @see  CPPropagStrength
     */
    public BinaryKnapsack(CPBoolVar [] b, final int [] weights, int load) {
         this(b,weights,CPIntVar.apply(b[0].store(),load,load));
    }

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		if (n > 0) {
            s().post(new BinaryKnapsackWithCardinality(x,w,c,n));
        }

		s().post(new LightBinaryKnapsack(x,w,c));
		if (l == CPPropagStrength.Weak) {
			deactivate();
			return;
		}
		
		candidate = new ReversibleInt[x.length];
		for (int i = 0; i < candidate.length; i++) {
			candidate[i] = new ReversibleInt(s(),0);
		}
		
		int S = 0;
		for (int i = 0; i < w.length; i++) {
			S += w[i];
			candidate[i].setValue(1);
		}
		
		rcap = new ReversibleInt(s(),0);
		pcap = new ReversibleInt(s(),S);
		nb = new ReversibleInt(s(),x.length);
		
		for (int i = 0; i < x.length; i++) {
			if (x[i].isBound()) {
				if (x[i].isTrue()) {
					bind(i) ;
				}
				else {
					remove(i) ;
				}
			}
			else {
				x[i].callValBindIdxWhenBind(this, i); // valBindIdx
				x[i].callPropagateWhenDomainChanges(this); // propagate
			}
		}
		if (!c.isBound()) c.callPropagateWhenBoundsChange(this);
		
		alpha_ = 0;
		beta_ = 0;
		X = new int[x.length];
		
		propagate();
	}
	

	@Override
	public void valBindIdx(CPIntVar var, int idx) {
		if (var.getMin() == 1)
			bind(idx);
		else
			remove(idx);
	}

	
	private void bind(int i) {
		int wi = w[i];
		int nrcap = rcap.getValue() + wi;
		c.updateMin(nrcap) ;
		rcap.setValue(nrcap);
		candidate[i].setValue(0);
		nb.decr(); //nb--
	}
	
	
	private void remove(int i) {
		pcap.setValue(pcap.getValue() - w[i]);
		c.updateMax(pcap.getValue()) ;
		candidate[i].setValue(0);
		nb.decr();
	}
	
	
	
	@Override
	public void propagate() throws Inconsistency {
		this.alpha_ = 0;
		this.beta_ = 0;
		int leftover = c.getMax() - rcap.getValue();
		int slack = pcap.getValue() - c.getMin();
		for (int k = 0; k < x.length; k++) {
			if (candidate[k].getValue() == 1) {
				if (w[k] > leftover) {
					x[k].removeValue(1);
					return;
                }
				if (w[k] > slack) {
					x[k].assign(1);
					return;
                }
			}
		}

		boolean pruneMore = true;
		if (nb.getValue() <= 2)
			return;
		if (noSumPossible(c.min() - rcap.value(),c.getMax() - rcap.getValue()))
			throw Inconsistency.get();

		if (pruneMore) {
			int lastsize = -1;
			for(int k = 0; k < x.length; k++) {
				if (candidate[k].getValue() == 1 && w[k] != lastsize) {
					lastsize = w[k];
					candidate[k].setValue(0);
					boolean toremove = noSumPossible(Math.max(c.getMin(),rcap.getValue()+w[k]) - rcap.getValue() - w[k], c.getMax() - rcap.getValue() - w[k]);
					candidate[k].setValue(1);
					if (toremove) {
						x[k].removeValue(1);
						return;
					}
				}
			}
			lastsize = -1;
			for(int k = 0; k < x.length; k++) {
				if (candidate[k].getValue()==1 && w[k] != lastsize) {
					lastsize = w[k];
					candidate[k].setValue(0);
					boolean toinsert = noSumPossible(c.getMin() - rcap.getValue(),
							Math.min(c.getMax(),pcap.getValue() - w[k]) - rcap.getValue());
					candidate[k].setValue(1);
					if (toinsert) {
						x[k].assign(1) ;
					}
				}
			}
		}
		if(noSumPossible(c.getMin() - rcap.getValue(),c.getMin() - rcap.getValue())){
			c.updateMin( rcap.getValue()+beta_) ;
		}
		if(noSumPossible(c.getMax() - rcap.getValue(),c.getMax() - rcap.getValue())){
            c.updateMax(rcap.getValue()+alpha_) ;
		}
	}



	private boolean noSumPossible(int alpha,int beta) {
	   assert(alpha <= beta);

	   if (alpha <= 0 || beta >= pcap.getValue()) {
           return false;
       }

		int Xs = 0;
		for (int i = 0; i < x.length; i++){
			if(candidate[i].getValue() == 1) Xs++;
		}
		
		int sumX = 0;
		int l = 0;
		for (int i = 0; i < Xs; i++) {
			while (candidate[l].getValue() == 0){
				l++;
			}
			X[i] = w[l];
			sumX += X[i];
			l++;
		}
		
		if(beta >= sumX) return false;

		int Sa = 0;
		int Sb = 0;
		int Sc = 0;
		int k = 0;
		int k_ = 0;

		while (Sc+X[Xs-k_-1] < alpha) {
			Sc += X[Xs-k_-1];
			k_++;
		}
		Sb = X[Xs-k_-1];
		while (Sa<alpha && Sb<=beta) {
			k++;
			Sa += X[k-1];
			if (Sa < alpha) {
				k_--;
				Sb += X[Xs-k_-1];
				Sc -= X[Xs-k_-1];
				while (Sa+Sc >= alpha) {
					k_--;
					Sc -= X[Xs-k_-1];
					Sb += X[Xs-k_-1] - X[Xs-k_-k-1-1];
				}
			}
		}
		alpha_ = Sa + Sc;
		beta_ = Sb;
		return Sa < alpha;
	}
}
