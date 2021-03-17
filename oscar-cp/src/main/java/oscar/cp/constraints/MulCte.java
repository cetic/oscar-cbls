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
import oscar.cp.util.NumberUtils;
import scala.collection.Iterable;
import scala.collection.immutable.List;

import java.util.Arrays;
import java.util.LinkedList;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Multiplication Constraint x * c = z
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MulCte extends Constraint {

	private CPIntVar x, z;
	private int c;

    /**
     * x * c == z
     * @param x
     * @param c
     * @param z
     * @see CPIntVar#mul(int)
     */
	public MulCte(CPIntVar x, int c, CPIntVar z) {
		super(x.store(),"MulCte");
		this.x = x;
		this.z = z;
		this.c = c;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		return CollectionConverters.asScala(Arrays.asList(x, z));
	}

	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		propagate();
		if (isActive()) {
			x.callPropagateWhenBoundsChange(this);
			z.callPropagateWhenBoundsChange(this);
		}
		/*
		if (l == CPPropagStrength.Strong) {
			if (x.getSize() <= 100) { // remove all numbers not multiples of c if dom size to too big
				for (int v = z.getMin(); v <= z.getMax(); v++) {
					if (z.hasValue(v) && (v%c != 0)) {
						z.removeValue(v);
					}
				}
			}
		}*/
	}
	
	@Override
	public void propagate() {
		if (x.isBound()) {
			z.assign(NumberUtils.safeMul(c , x.min()));
			deactivate();
		}
		else if (c == 0) {
				z.assign(0);
                deactivate();
        }
        else {
            z.updateMin(Math.min(NumberUtils.safeMul(c , x.getMin()), NumberUtils.safeMul(c , x.getMax())));
            z.updateMax(Math.max(NumberUtils.safeMul(c , x.getMin()), NumberUtils.safeMul(c , x.getMax())));
            x.updateMin(Math.min(NumberUtils.ceilDiv(z.getMin(), c),  NumberUtils.ceilDiv(z.getMax(), c)));
            x.updateMax(Math.max(NumberUtils.floorDiv(z.getMin(), c), NumberUtils.floorDiv(z.getMax(), c)));
        }
	}
}
