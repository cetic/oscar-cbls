package oscar.cp.constraints;

import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import oscar.algo.reversible.ReversibleInt;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

public class BinaryKnapsackWithCardinality extends Constraint {

	CPBoolVar [] x;
	int [] w;
	CPIntVar c;
    int n;

    ReversibleInt packed;
    ReversibleInt nPacked;

	public BinaryKnapsackWithCardinality(CPBoolVar [] b, final int [] weights, CPIntVar load, int nbItems) {
		super(b[0].store(),"BinaryKnapsackWithCardinality");

		Integer [] perm = new Integer [weights.length];
		for (int i = 0; i < perm.length; i++) {
			if (weights[i] < 0) {
				throw new RuntimeException("weights must be non negative");
			}
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
        n = nbItems;
		for (int i = 0; i < x.length; i++) {
			w[i] = weights[perm[i]];
			x[i] = b[perm[i]];
		}
	}

    @Override
    public Iterable<CPVar> associatedVars() {
        List<CPVar> l = new LinkedList<>(Arrays.asList(x));
        l.add(c);
        return CollectionConverters.asScala(l);
    }

	@Override
	public void setup(CPPropagStrength l) {

        packed = new ReversibleInt(s(),0);
        nPacked = new ReversibleInt(s(),0);
        for (int i = 0; i < x.length; i++) {
            if (x[i].isBound()) {
                packed.setValue(packed.getValue() + w[i]);
                nPacked.incr();
            } else {
                x[i].callValBindIdxWhenBind(this,i);
                x[i].callPropagateWhenBind(this);
            }

        }
	}

	@Override
	public void valBindIdx(CPIntVar var, int idx) {
        if (var.getMin() == 1) {
            nPacked.incr();
            packed.setValue(packed.getValue() + w[idx]);
        }
	}


	@Override
	public void propagate() {
        int curn = nPacked.getValue();
        int curw = packed.getValue();
        for (int i = 0; i < x.length && curn < n; i++) {
            if (!x[i].isBound()) {
                curw += w[i];
                curn++;
            }
        }
        c.updateMax(curw);

        curn = nPacked.getValue();
        curw = packed.getValue();
        for (int i = x.length-1; i >=0  && curn < n; i--) {
            if (!x[i].isBound()) {
                curw += w[i];
                curn++;
            }
        }

        c.updateMin(curw);
	}
}