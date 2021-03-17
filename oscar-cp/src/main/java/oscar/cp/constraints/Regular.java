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
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import oscar.algo.Inconsistency;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;


/**
 * Regular Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Regular extends Constraint {

	private int nbStates;
	private int [][] T; //transition matrix;
	private int initialState;
	private Set<Integer> acceptingStates;
	
	private CPIntVar [] x;
	private CPIntVar [] q;

	@Override
	public Iterable<CPVar> associatedVars() {
		List<CPVar> l = new LinkedList<>(Arrays.asList(x));
		l.addAll(Arrays.asList(q));
		return CollectionConverters.asScala(l);
	}

    /**
     * Constraint x to be a valid sequence accepted by the automaton
     * @param x
     * @param automaton
     */
	public Regular(CPIntVar [] x, Automaton automaton) {
		super(x[0].store(),"Regular");
		this.x = x;

	    automaton.setPosted();//ensure that the automaton is not modified anymore
	    nbStates = automaton.getNbStates();
	    T = automaton.getTransitionMatrix();//transition matrix
	    initialState = automaton.getInitialState();
	    acceptingStates = automaton.getAcceptingStates();
	    q = new CPIntVar[x.length];
	    for (int i = 0; i < q.length; i++) {
			q[i] = CPIntVar.apply(s(),0,nbStates-1);
		}		
	}

	
	@Override
	public void setup(CPPropagStrength l) throws Inconsistency {
		s().post(ElementCst2D.apply(T,CPIntVar.apply(s(),initialState,initialState),x[0],q[0]));
		
		for (int v = 0; v < nbStates; v++) {
			if (!acceptingStates.contains(v)) {
				q[x.length-1].removeValue(v);
			}
		}
		for (int i = 1; i < x.length; i++) {
			s().post(ElementCst2D.apply(T,q[i-1],x[i],q[i]));
		}
	}

}
