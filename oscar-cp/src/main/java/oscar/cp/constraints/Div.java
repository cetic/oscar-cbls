/*
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cp.constraints;

import oscar.algo.Inconsistency;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;


/**
 * Div Constraint: x div v = y
 * Crappy implementation.
 * @author Gustav Bjordal
 */
public class Div extends Constraint {

    private CPIntVar x;
    private CPIntVar y;
    private int v;

    /**
     * Creates a modulo constraint x % v = y
     * @param x
     * @param v a value > 0
     * @param y
     */
    public Div(CPIntVar x, int v, CPIntVar y) {
        super(x.store(),"Div");
        assert( v != 0);
        if (v == 0) throw new RuntimeException("v must be > 0");
        this.x = x;
        this.v = v;
        this.y = y;
    }

    @Override
    public Iterable<CPVar> associatedVars() {
        List<CPVar> l = new LinkedList<>(Arrays.asList(x, y));
        return CollectionConverters.asScala(l);
    }

    @Override
    public void setup(CPPropagStrength l) throws Inconsistency {
        if(v < 0){
            y.updateMin(x.max()/v);
            y.updateMax(x.min()/v);

            x.updateMin(y.max()*v);
            x.updateMax(y.min()*v);
        }else{
            y.updateMin(x.min()/v);
            y.updateMax(x.max()/v);

            x.updateMin(y.min()*v);
            x.updateMax(y.max()*v);
        }


        if (!x.isBound()) x.callValBindWhenBind(this);
        if (!y.isBound()) y.callValRemoveWhenValueIsRemoved(this);
        if (!x.isBound()) x.callValRemoveWhenValueIsRemoved(this);
    }


    @Override
    public void valRemove(CPIntVar var, int val) {
        if(var == y){
            int sgn = v>0?1:-1;
            for (int i = 0; i < v; i++) {
                x.removeValue(val*v+i*sgn);
            }
        }else{
            if(v>0) {
                if (val > x.max() || val < x.min()) {
                    y.updateMin(x.min()/v);
                    y.updateMax(x.max()/v);
                }
            }else{
                if (val > x.max() || val < x.min()) {
                    y.updateMin(x.max() / v);
                    y.updateMax(x.min() / v);
                }
            }
        }
    }

    @Override
    public void valBind(CPIntVar x) throws Inconsistency {
        y.assign(x.min()/v);
    }
}
