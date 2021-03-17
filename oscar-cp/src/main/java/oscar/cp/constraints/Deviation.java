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
import scala.collection.Iterable;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Deviation Constraint, a constraint for the average absolute deviation to the mean. <br>
 * See paper: Bound Consistent Deviation Constraint, Pierre Schaus et. al. , CP07
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Deviation extends Constraint {


    private CPIntVar [] x;
    private int n; // length of x
    private CPIntVar nd;
    private int s;


    // some structures used during propagation
    private int [] nx; //nx optimal solution
    // scaled upper and upper bounds values of x
    private int [] nxmin;
    private int [] nxmax;
    private int nmaxsum;
    private int nminsum;

    private int [] overlaps;
    private int [] maximum;
    private int [] overlaps_sup;

    /**
     * Post the constraint that n * sum_i |x[i]-s/n| <= nd and sum_i x[i] = s <br> 
     * @param x
     * @param s
     * @param nd
     */
    public Deviation(CPIntVar [] x, int s, CPIntVar nd) {
        super(x[0].store(),"Deviation");
        assert (x.length >= 2);
        this.x = x;
        this.nd = nd;
        this.n = x.length;
        this.s = s;
        nx = new int [n];
        nxmin = new int [n];
        nxmax = new int [n];
        overlaps = new int [n];
        maximum = new int [n];
        overlaps_sup = new int [n];
    }

    @Override
    public Iterable<CPVar> associatedVars() {
        List<CPVar> l = new LinkedList<>(Arrays.asList(x));
        l.add(nd);
        return CollectionConverters.asScala(l);
    }

    @Override
    public void setup(CPPropagStrength l) throws Inconsistency {
    	// post the decomposition
    	CPIntVar [] devVar = new CPIntVar[n];
    	for (int i = 0; i < x.length; i++) {
    		devVar[i] = oscar.cp.modeling.constraint.absolute(oscar.cp.modeling.constraint.minus(oscar.cp.modeling.constraint.mul(x[i],n),s));
    	}
    	s().post(new Sum(devVar,nd));
    	
    	//add(vari == sum(periods)(p => (l(p)*nbPeriods - credits.sum).abs))
    	
    	
        for (int i = 0; i < x.length; i++) {
            if (!x[i].isBound())
                x[i].callPropagateWhenBoundsChange(this);
        }
        nd.callPropagateWhenBoundsChange(this);
        propagate();
    }

    @Override
    public void propagate() {
        // 1) make the sum constraint bound consistent
        propagateSum();
        // 2) initialize scaled domain data
        initData(false);
        
        // 3) compute an assignment of minimum deviation
        computeMinDevAssignment();
        // 4) compute the deviation of this assignment and prune the lower bound of the deviation var
        int delta_min = computeMinDev();
        nd.updateMin(delta_min);
        // 5) propagate the upper and lower bounds of the x[i]'s
        propagateBounds(delta_min);
        //propagateBoundsShaving(); // replace previous line with this one to use the shaving (debugging purpose, much less efficient)
    }

    /**
     * Bound Consistent propagation for the sum constraint sum x[i] == s
     * @return Suspend if the sum is bound consistent, false otherwise
     */
    private void propagateSum() {
        int maxsum = 0;
        int minsum = 0;
        for (int i = 0; i < x.length; i++) {
            maxsum += x[i].getMax();
            minsum += x[i].getMin();
        }
        for (int i = 0; i < x.length; i++) {
            x[i].updateMax(s - (minsum - x[i].getMin()));
            x[i].updateMin(s - (maxsum - x[i].getMax()));
        }
    }

    /**
     *
     * @param mirror true if we consider -x instead of x
     */
    private void initData(boolean mirror) {
        nmaxsum = 0;
        nminsum = 0;
        for (int i = 0; i < x.length; i++) {
            nxmax[i] = n *  (mirror ? -x[i].getMin(): x[i].getMax());
            nxmin[i] = n *  (mirror ? -x[i].getMax(): x[i].getMin());
            nmaxsum += nxmax[i];
            nminsum += nxmin[i];
        }
    }

    /**
     * return sum_i |nx[i]-s|
     */
    private int computeMinDev() {
        int res = 0;
        for (int i = 0; i < n; i++) {
            res += Math.abs(nx[i] - s);
        }
        return res;
    }

    private void computeMinDevAssignment() {
        int sum = 0;
        int sinf = s >= 0 ? s - s%n :   s - (n - (-s % n)); // nearest (multiple of s) <= s
        int ssup = sinf + n; // nearest (multiple of s) > s
        int s_nearest = (ssup - s <= s - sinf) ? ssup : sinf; // nearest multiple of n from s
        int nbOverlaps = 0; // number of overlapping variables
        // greedy assignment
        for (int i = 0; i < n; i++) {
            if (nxmin[i] >= s){
                nx[i] = nxmin[i];
            }
            else if (nxmax[i] <= s) {
                nx[i] = nxmax[i];
            }
            else { // overlapping variable nxmax[i] < s < nxmin[i]
                nx[i] = s_nearest;
                if (s % n != 0) overlaps[nbOverlaps++] = i;
            }
            sum += nx[i];
        }
        // repair to get the correct sum
        int delta = sum > n*s ? -n : n; // step used to make the repair
        // change overlapping variables
        for (int j = 0; j < nbOverlaps && sum != n * s; j++) { // as long as the sum is not satisfied and there are overlapping vars, we use that ones to repair
            nx[overlaps[j]] += delta;
            sum += delta;
        }
        // change other variables if the sum is still not satisfied
        for (int i = 0; i < n && sum != n * s; i++) {
            int nxi = nx[i];
            if (sum < n * s) { // increase nx[i] as much as possible to fix the too low sum
                nx[i] += n * s - sum;
                nx[i] = Math.min(nx[i],nxmax[i]);
            }
            else { // decrease nx[i] as much as possible to fix the too high sum
                nx[i] -= (sum - n * s);
                nx[i] = Math.max(nx[i],nxmin[i]);
            }
            sum += nx[i] - nxi;
        }
    }

    /**
     * Shaving computation of bnd consistent value of x[j] using the procedure to find the min deviation.
     * We start from nx and increase it by one (i.e. n in scaled domain) until the max allowed deviation is reached.
     * @param j the variable index
     * @param upper = true to get the maximum bnd consistent value for var x[j] <br>
     *        upper = false to get the  minimum bnd consistent value for var x[j]
     *
     * @return the extreme (upper or lower) bound consistent value for x[j]
     */
    private int boundConsistentValue(int j, boolean  upper) {
        initData(false);
        computeMinDevAssignment();
        int mindev = computeMinDev();
        int currval = nx[j];
        while (mindev <= nd.getMax()) {
            currval += upper ? n : -n;
            nmaxsum = 0;
            nminsum = 0;
            for (int i = 0; i < x.length; i++) {
                nxmax[i] = j==i ? currval : n *   x[i].getMax();
                nxmin[i] = j==i ? currval : n *   x[i].getMin();
                nmaxsum += nxmax[i];
                nminsum += nxmin[i];
            }
            if (n * s >= nminsum && n * s <= nmaxsum) {
                computeMinDevAssignment();
                mindev = computeMinDev();
            } else {
                break;
            }
        }
        if (upper)
            return (currval - n) / n;
        else
            return (currval + n) / n;
    }

    /**
     * Propagate the bound of the variable with a shaving using the computation of the min deviation
     */
    private void propagateBoundsShaving(){
        for (int i = 0; i < n; i++) {
            if (x[i].isBound()) continue;
            int max = boundConsistentValue(i,true);
            x[i].updateMax(max); //should never fail since it is called when the constraint is consistent
            int min = boundConsistentValue(i,false);
            x[i].updateMin(min); //should never fail since it is called when the constraint is consistent
        }
    }

    /**
     * Propagate the upper and lower bounds of x[i]'s <br>
     * Assume the constraint is consistent:
     * - the sum constraint is consistent
     * - min deviation smaller than max allowed deviation
     * @param min_delta is the minimum possible deviation
     */
    private void propagateBounds(int min_delta) {
        propagateBounds(min_delta, true); // filter upper bounds
        propagateBounds(min_delta, false); // filter lower bounds
    }

    /**
     *
     * @param val a positive or negative number
     * @param div a positive or negative number
     * @return floor(val/div)
     */
    private int divFloor(int val, int div) {
        assert(div > 0);
        int res = val / div;
        if (val < 0 && val%div !=0) res--;
        return res;
    }

    /**
     * Prune the upper/lower-bound of x's variables
     * Rmq: we apply a mirroing of the domains wrt 0 to prune the lower bounds  such that
            we can use the same algo to prune both sides of the domains
     * @param min_delta
     * @param upperBounds = true to prune the upper bounds of x's, false to prune the lower bounds.
     */
    private void propagateBounds(int min_delta, boolean upperBounds) {

    	initData(!upperBounds); // mirror = true if want to propagate lower bounds   
        
        int s = (!upperBounds ? -this.s : this.s);

        assert (n * s >= nminsum && n * s <= nmaxsum); // sum constraint should be consistent

        int sum = 0;
        int sinf = s >= 0 ? s - s%n :   s - (n - (-s % n)); // nearest (multiple of s) <= s
        int ssup = sinf + n; // nearest (multiple of s) > s
        int s_nearest = (ssup - s <= s - sinf) ? ssup : sinf; // nearest multiple of n from s
        int nbOverlaps = 0; // overlaps size
        
        assert(sinf%n == 0 && sinf <= s && (s-sinf < n));
        
        // greedy
        for (int i = 0; i < n; i++) {
            if (nxmin[i] >= s) {
                nx[i] = nxmin[i];
            }
            else if (nxmax[i] <= s) {
                nx[i] = nxmax[i];
            }
            else { // overlapping variable i.e. nxmax[i] < s < nxmin[i]
                nx[i] = s_nearest;
                if (s_nearest != s) overlaps[nbOverlaps++] = i; // overlapping only if not on s
            }
            sum += nx[i];
        }
        ////////////////////////////////////////////////////
        //computation of key values for the pruning;      //
        // - overlaps_sup                                 //
        // - maximum[i]                                   //
        ////////////////////////////////////////////////////
        if (sum == n * s) {
            int nboverlapssup = 0;
            if (s_nearest == ssup) nboverlapssup = nbOverlaps;
            for (int i = 0; i < n; i++) {
                maximum[i] = nx[i];
                if (nxmin[i] < s && nxmax[i] > s && s_nearest == ssup &&  s % n != 0) {
                    overlaps_sup[i] = nboverlapssup - 1;
                }
                else overlaps_sup[i] = nboverlapssup;
            }
        }
        else{
            if ((sum > n * s && s_nearest == ssup) || (sum < n * s && s_nearest == sinf)) {
                int delta = sum > n * s ? -n : n;
                for (int j = 0; j < nbOverlaps && sum != n * s ; j++){
                    nx[overlaps[j]] += delta;
                    sum += delta;
                }
            }
            int nboverlapssup = 0;
            for (int i = 0; i < n; i++) {
                if (nx[i] > nxmin[i] && nx[i] == ssup && s % n != 0) nboverlapssup++;
            }
            if (sum == n * s) {
                for (int i = 0; i < n; i++) {
                    boolean overlapi = (nxmin[i] < s && nxmax[i] > s);
                    if (overlapi && nboverlapssup > 0) {
                        maximum[i] = ssup;
                        overlaps_sup[i] = nboverlapssup - 1;
                    }
                    else {
                        maximum[i] = nx[i];
                        overlaps_sup[i] = nboverlapssup;
                    }
                }
            }
            else if (sum > n * s) {
                // nx[i] = sinf or nx[i] <= s (there is no more overlappingsup)
                for (int i = 0; i < n; i++) {
                    maximum[i] = nx[i];
                    overlaps_sup[i] = 0;
                }
            }
            else { // sum < n * s
                for (int i = 0; i < n; i++) {
                    boolean overlapi = (nxmin[i] < s && nxmax[i] > s && nboverlapssup > 0);
                    if (overlapi) overlaps_sup[i] = nboverlapssup - 1;
                    else          overlaps_sup[i] = nboverlapssup;

                    if (nx[i] < nxmax[i])	maximum[i] = nx[i] + (n * s - sum);
                    else 					maximum[i] = nx[i];
                }
            }
        }
        ///////////////////////////////////////////////////
        //pruning of upper bound of x[i] for i in [1..n] //
        ///////////////////////////////////////////////////
        int increase_down_up = (ssup - s) - (s - sinf);
        int increase_up_down = -increase_down_up;
        for (int i = 0; i < n; i++) {
            if (x[i].isBound() || maximum[i] >= nxmax[i]) continue ;
            int maxval = maximum[i];
            int deltamin = min_delta;

            if (overlaps_sup[i] > 0 && (deltamin + overlaps_sup[i] * (n + increase_up_down) >= nd.getMax())) {
                int delta = nd.getMax() - deltamin;
                maxval += (n * delta) / (n + increase_up_down);
                // bound = floor(maxval/n)
                int bound = divFloor(maxval,n); // floor(maxval/n)
                pruneBound(x[i],bound,!upperBounds);
                continue;
            }
            else {
                if (maxval == sinf && sinf < s) {
                    assert (overlaps_sup[i] == 0);
                    deltamin += n + increase_down_up;
                    if (deltamin > nd.getMax()){
                        assert (maxval % n == 0);
                        int bound = maxval/n;
                        pruneBound(x[i],bound,!upperBounds);
                        continue;
                    }
                    maxval += n;
                }
                deltamin += overlaps_sup[i] * (n + increase_up_down);
                maxval += n * overlaps_sup[i];
                //slope of 2 x n
                int delta = nd.getMax() - deltamin;
                maxval +=  delta / 2; // n * delta / (2 * n);
                int bound = divFloor(maxval,n); // floor(maxval/n)
                pruneBound(x[i],bound,!upperBounds);
                continue;
            }
        }
    }

    /**
     * @param x
     * @param bound
     * @param mirror
     */
    private void pruneBound(CPIntVar x, int bound, boolean mirror) {
        if (!mirror) {
            x.updateMax(bound);
        } else {
            x.updateMin(-bound);
        }
    }

}
