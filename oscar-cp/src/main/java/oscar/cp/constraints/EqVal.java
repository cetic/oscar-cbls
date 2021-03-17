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

import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Equality Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class EqVal extends Constraint {

	CPIntVar x;
	int v;

    /**
     * Constraint x to take value v
     * @param x
     * @param v
     */
	public EqVal(CPIntVar x, int v) {
		super(x.store(),"EqVal");
		this.x = x;
		this.v = v;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = Collections.singletonList(x);
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l) {
		x.assign(v);
	}
	

}



