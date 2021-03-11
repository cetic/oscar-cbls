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
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.List;

public class Garded extends Constraint {
	
	private CPBoolVar b;
	private Constraint c;
	private boolean onTrue;
	
	/**
	 * Garded constraint: c is posted only when b becomes true (if onTrue) or when b becomes false (if onFalse)
	 * @param b
	 * @param c
	 * @param onTrue 
     * @see  Constraint#when(cp.core.CPBoolVar)
	 */
	public Garded(CPBoolVar b, Constraint c, boolean onTrue) {
		super(b.store(),"Garded Constraint");
		this.b = b;
		this.c = c;
		this.onTrue = onTrue;
	}

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = CollectionConverters.asJava(c.associatedVars().toList());
		l.add(b);
		return CollectionConverters.asScala(l);
	}

	@Override
	public void setup(CPPropagStrength l)  throws Inconsistency {
		if (!b.isBound()) {
			b.callPropagateWhenBind(this);
		} else {
			if ((b.min() == 1 && onTrue) || (b.min() == 0 && !onTrue))
				s().post(c);
			deactivate();
		}	
	}
	
	@Override
	public void propagate() throws Inconsistency {
		if ((b.min() == 1 && onTrue) || (b.min() == 0 && !onTrue))
			s().post(c);
		deactivate();
	}

}
