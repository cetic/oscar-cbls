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
 *******************************************************************************/

package oscar.cp.constraints;

import oscar.algo.Inconsistency;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.Constraint;
import oscar.algo.reversible.*;
import oscar.cp.core.delta.DeltaIntVar;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.variables.CPIntVarAdaptable;
import oscar.cp.core.variables.CPVar;
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Minimum Assignment (or weighted matching)
 * @author Pierre Schaus pschaus@gmail.com
 */
public class MinAssignment extends Constraint {

    private int n;
    private CPIntVar[] x;
    private CPIntVar cost;
    private int [][] weights;

    private ReversibleInt [] lc;
    private ReversibleInt [] lr;
    private ReversibleInt [] valc; // row assigned to the column
    private ReversibleInt [] valr; // column assign to the row
    private ReversibleInt [][] w;

    private boolean [] markc;
    private boolean [] markr;
    private int [] pi;
    private int [] pathRowOfColumn;

    private int M;

    private final int NONE = Integer.MAX_VALUE;


    private int [] values; // used to fill in domain
    private int[][] sp; // shortest path dist in residual graph
    private int [] distance; // used by bellman-ford

    private boolean [] isValueBound;
    private int [] unboundVars;
    private int [] unboundVals;
    // cached values for valr and valc
    private int [] valr_;
    private int [] valc_;
    private int nUnboundVars = 0;
    private int nUnboundVals = 0;



    private DeltaIntVar[] delta;

    private boolean exactReducedCosts = false;


    /**
     *
     * @param x vector of variables taking their values on 0..n-1
     * @param weightMat a n x n matrix
     * @param cost >= sum_i weightMat[i][x[i]] with all values of x different
     */
    public MinAssignment(CPIntVar [] x, int [][] weightMat, CPIntVar  cost) {
        super(x[0].store(),"minassingmentjava");

        if (weightMat.length != x.length) throw new IllegalArgumentException("MinAssignment: dim of x and weights must match");



        n = weightMat[0].length;
        this.x = new CPIntVar[n];
        for (int i = 0; i < n; i++) {
            if (i < x.length) this.x[i] = x[i];
            else this.x[i] = new CPIntVarAdaptable(s(), 0, n-1, true, "x_minAssignment" + i);
        }



        this.priorityL2_$eq(s().MaxPriorityL2()-3);

        this.cost = cost;
        this.values = new int[n];


        initWeightMatrix(weightMat);

        sp = new int[n][n];
        distance = new int[2*n];

        isValueBound = new boolean[n];
        unboundVars = new int[n];
        unboundVals = new int[n];
        nUnboundVars = 0;
        nUnboundVals = 0;
        valr_ = new int[n];
        valc_ = new int[n];

        delta = new DeltaIntVar[n];

    }

    @Override
    public Iterable<CPVar> associatedVars() {
        return CollectionConverters.asScala(Arrays.asList(x));
    }

    private void initWeightMatrix(int [][] weightMat) throws RuntimeException {




        this.weights = new int[x.length][];
        for (int i = 0; i < x.length; i++) {
            if (i < weightMat.length && weightMat[i].length != x.length) {
                throw new RuntimeException("weightMat should be a square "+x.length+"x"+x.length+" matrix");
            }
            weights[i] = new int[x.length];
            if (i < weightMat.length) {
                for (int j = 0; j < weightMat[i].length; j++) {
                    weights[i][j] = weightMat[i][j];
                }
            }
        }
    }

    private void initTrails() {

        M = Integer.MIN_VALUE;
        w = new ReversibleInt [x.length][];
        for (int i = 0; i < x.length; i++) {
            w[i] = new ReversibleInt[x.length];
            for (int j = 0; j < x.length; j++) {
                w[i][j] = new ReversibleInt(s(),0);
                w[i][j].setValue(weights[i][j]);
                M = Math.max(M, weights[i][j]);
            }
        }
        M++;

        for (int i = 0; i < x.length; i++) {
            for (int j = 0; j < x.length; j++) {
                if (!x[i].hasValue(j)) {
                    w[i][j].setValue(M);
                }
            }
        }

        lc = new ReversibleInt [x.length];
        lr = new ReversibleInt [x.length];
        valc = new ReversibleInt [x.length];
        valr = new ReversibleInt [x.length];

        for (int i = 0; i < lc.length; i++) {
            lc[i] = new ReversibleInt(s(),0);
            lc[i].setValue(0);

            lr[i] = new ReversibleInt(s(),0);
            lr[i].setValue(0);

            valc[i] = new ReversibleInt(s(),NONE);
            valc[i].setValue(NONE);
            valr[i] = new ReversibleInt(s(),NONE);
            valr[i].setValue(NONE);
        }
    }


    @Override
    public void setup(CPPropagStrength l) throws Inconsistency {
        initTrails();

        markc = new boolean[x.length];
        markr = new boolean[x.length];
        pi = new int[x.length];
        pathRowOfColumn = new int[x.length];

        reduceMatrix();
        initAssignment();
        findMinimalAssignment();
        updateUnBounds();
        prune() ;

        for (int i = 0; i < x.length; i++) {
            if (!x[i].isBound()) {
                delta[i] = x[i].callPropagateOnChangesWithDelta(this);
            }
        }
        if (!cost.isBound()) {
            cost.callPropagateWhenBoundsChange(this);
        }

        if (l == CPPropagStrength.Strong) {
            exactReducedCosts = true;
        }
    }

    private void reduceMatrix() {
        for (int j = 0; j < x.length; j++) {
            int m = Integer.MAX_VALUE;
            for (int i = 0; i < x.length; i++) {
                int val = w[i][j].getValue() - lc[j].getValue() - lr[i].getValue();
                m = Math.min(m, val);
            }
            lc[j].setValue(lc[j].getValue() + m);
        }
        for (int i = 0; i < x.length; i++) {
            int m = Integer.MAX_VALUE;
            for (int j =0; j< x.length; j++) {
                int val = w[i][j].getValue() - lc[j].getValue() - lr[i].getValue();
                m = Math.min(m, val);
            }
            lr[i].setValue(lr[i].getValue() + m);
        }
    }

    private void initAssignment() {
        for (int i = 0; i < x.length; i++) {
            for (int j = 0; j < x.length; j++) {
                if (!colAssigned(j) && w[i][j].getValue() == 0) {
                    assignRow(i,j);
                    break;
                }
            }
        }
    }

    private void findMinimalAssignment() {
        for(int i = 0; i < x.length; i++)
            if (!rowAssigned(i))
                applyAssignment(i,findPath(i));
    }

    private int findPath(int i) {
        for (int j = 0; j < x.length; j++) {
            markr[j] = false;
            markc[j] = false;
        }
        markr[i] = true;
        for (int c = 0; c < x.length; c++) {
            pi[c] = w[i][c].getValue() - lc[c].getValue() - lr[i].getValue();
            pathRowOfColumn[c] = i;
        }
        do {
            int col = - 1;
            int row = - 1;
            // find an arc i->j with zero reduced cost
            for (int c = 0; c < x.length; c++) {
                if (!markc[c] && pi[c] == 0) {
                    col = c;
                    break;
                }
            }
            // dual step
            if (col < 0) {
                // no, zero reduced cost arc found so we reduce to introduce new zero-cost edge
                int m = Integer.MAX_VALUE;
                for (int c = 0; c < x.length; c++) {
                    if (!markc[c]) {
                        if (pi[c] < m) {
                            m = pi[c];
                            col = c;
                        }
                    }
                }
                for (int k = 0; k < x.length; k++) {
                    if (markr[k])
                        lr[k].setValue(lr[k].getValue() + m);
                    if (markc[k])
                        lc[k].setValue(lc[k].getValue() - m);
                    else
                        pi[k] -= m;
                }
            }
            assert(pi[col] == 0);

            // primal step
            if (colAssigned(col)) {
                row = valc[col].getValue();
                markr[row] = true;
                markc[col] = true;
            } else {
                // augmenting path
                return col;
            }

            // update the minimum dual values pi and the (future) path if a column is selected

            for (int c = 0; c < x.length; c++) {
                if (!markc[c]) {
                    int m = w[row][c].getValue() - lc[c].getValue() - lr[row].getValue();
                    if (m < pi[c]) {
                        pi[c] = m;
                        pathRowOfColumn[c] = row;
                    }
                }
            }

        } while (true);
    }

    //apply the changes on the alternating path found starting at r and finishing in c
    private void applyAssignment(int r,int c) {
        int row;
        do {
            row = pathRowOfColumn[c];
            valc[c].setValue(row);
            int col = valr[row].getValue();
            valr[row].setValue(c);
            c = col;
        } while (row != r);
    }



    private boolean rowAssigned(int i) {
        return valr[i].getValue() != NONE;
    }

    private boolean colAssigned(int j) {
        return valc[j].getValue() != NONE;
    }

    private void assignRow(int i, int j) {
        valr[i].setValue(j);
        valc[j].setValue(i);
    }

    @Override
    public void propagate() {
        // treat the deltas
        for (int r = 0; r < x.length; r++) {
            if (delta[r] != null) { // if variable was not already bound at posting
                int nRemoved = delta[r].fillArray(values);
                for (int j = 0; j < nRemoved; j++) {
                    int c = values[j];
                    w[r][c].setValue(M);
                    if (valr[r].getValue() == c) {
                        valr[r].setValue(NONE);
                        valc[c].setValue(NONE);
                    }
                }
            }
        }


        boolean valid = true;
        for (int i = 0; i < x.length; i++) {
            if (!rowAssigned(i)) {
                valid = false;
                break;
            }
        }
        if (!valid) {
            findMinimalAssignment();
        }
        for (int i = 0; i < x.length; i++) {
            valc_[i] = valc[i].getValue();
            valr_[i] = valr[i].getValue();
        }

        prune();
    }


    private void updateUnBounds() {
        Arrays.fill(isValueBound,false);
        nUnboundVars = 0;
        nUnboundVals = 0;
        for (int i = 0; i < x.length; i++) {
            if (!x[i].isBound()) {
                unboundVars[nUnboundVars++] = i;
            } else {
                isValueBound[x[i].min()] = true;
            }
        }
        for (int i = 0; i < x.length; i++) {
            if (!isValueBound[i]) {
                unboundVals[nUnboundVals++] = i;
            }
        }

    }


    public void updateAllPairsShortestPathFloydWarshall() {
        for (int i = 0; i < x.length; i++) {
            Arrays.fill(sp[i],M);
        }

        int [][] dist = new int[x.length+x.length][x.length+x.length];
        for (int i = 0; i < x.length+x.length; i++) {
            for (int j = 0; j < x.length+x.length; j++) {
                if (i == j) {
                    dist[i][j] = 0;
                } else if (i < x.length && j >= x.length && valr[i].getValue() != j-x.length) {
                    dist[i][j] = w[i][j-x.length].getValue();
                } else if (i >= x.length && j < x.length && valr[j].getValue() == i-x.length){
                    dist[i][j] = -w[j][i-x.length].getValue();
                } else {
                    dist[i][j] = M;
                }
            }
        }
        for (int k = 0; k < x.length+x.length; k++) {
            for (int i = 0; i < x.length+x.length; i++) {
                for (int j = 0; j < x.length+x.length; j++) {
                    if (dist[i][j] > dist[i][k] + dist[k][j]) {
                        dist[i][j] = dist[i][k] + dist[k][j];
                    }
                }
            }
        }


        for (int s = 0; s < nUnboundVars; s++) {
            int i = unboundVars[s];
            for (int ind = 0; ind < nUnboundVals; ind++) {
                int j = unboundVals[ind];
                sp[i][j] = dist[i][j+x.length];
            }
        }

    }


    public void updateAllPairsShortestPathBellmanFord() {
        for (int i = 0; i < x.length; i++) {
            Arrays.fill(sp[i],M);
        }

        for (int s = 0; s < nUnboundVars; s++) {
            int source = unboundVars[s];
            Arrays.fill(distance,M);
            distance[source] = 0;
            //System.out.println("init distance from source "+source+":"+Arrays.toString(distance));
            for (int k = 0; k < (nUnboundVars + nUnboundVals)-1; k++) {
                // edges from left to right (variable to values)
                for (int l = 0; l < nUnboundVars; l++) {
                    int i = unboundVars[l];
                    int nVals = x[i].fillArray(values);
                    for (int ind = 0; ind < nVals; ind++) {
                        int j = values[ind];
                        if (valr_[i] != j) {
                            // there is an edge i->j in residual graph
                            if (distance[i] + weights[i][j] < distance[x.length + j]) {
                                distance[x.length + j] = distance[i] + weights[i][j];
                            }

                        }
                    }
                }
                // edges from right to left (matched values to variables)
                for (int l = 0; l < nUnboundVals; l++) {
                    int i = unboundVals[l];
                    int j = valc_[i];
                    if (distance[i + x.length] - weights[j][i] < distance[j]) {
                        distance[j] = distance[i + x.length] - weights[j][i];
                    }
                }
            }
            for (int l = 0; l < nUnboundVals; l++) {
                int c = unboundVals[l];
                sp[source][c] = distance[c + x.length];
            }
            //System.out.println("distance from source "+source+":"+Arrays.toString(distance));
        }

    }


    private void prune() {
        int sum = 0;
        for (int i = 0; i < x.length; i++) {
            sum += lc[i].getValue();
            sum += lr[i].getValue();
        }
        cost.updateMin(sum) ;
        int slack = cost.getMax() - sum;
        pruneLPReducedCosts(slack) ;
        if (exactReducedCosts)
            pruneExactReducedCosts(slack);
    }

    public void pruneLPReducedCosts(int slack) {
        for (int s = 0; s < nUnboundVars; s++) {
            int i = unboundVars[s];
            int nVals = x[i].fillArray(values);
            for (int ind = 0; ind < nVals; ind++) {
                int j = values[ind];
                if (valr_[i] != j) { //
                    int val = w[i][j].getValue();
                    int m = val - lc[j].getValue() - lr[i].getValue(); // get reduced cost of assigning i->j
                    if (m > slack) {
                        w[i][j].setValue(M);
                        x[i].removeValue(j);
                    }
                }
            }
        }
    }


    public void pruneExactReducedCosts(int slack) {


        //updateAllPairsShortestPathBellmanFord();
        updateAllPairsShortestPathFloydWarshall();


        for (int s = 0; s < nUnboundVars; s++) {
            int i = unboundVars[s];
            int nVals = x[i].fillArray(values);
            for (int ind = 0; ind < nVals; ind++) {
                int j = values[ind];
                if (valr_[i] != j) { // check if there is an arc

                    int i_p = valc_[j]; // origin of i
                    int j_p = valr_[i]; // destination of i

                    // exact reduced cost of assigning i->j
                    int m = (weights[i][j]+sp[i_p][j_p])-(weights[i][j_p]+weights[i_p][j]);

                    //int val = w[i][j].getValue();
                    //int mlp= val - lc[j].getValue() - lr[i].getValue();
                    //assert(mlp <= m); // exact reduced cost stronger than lp reduced cost

                    if (m > slack) {
                        w[i][j].setValue(M);
                        x[i].removeValue(j);
                    }
                }
            }
        }
    }




}
