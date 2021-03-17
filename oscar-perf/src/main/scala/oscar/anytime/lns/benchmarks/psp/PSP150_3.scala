package oscar.anytime.lns.benchmarks.psp

import oscar.anytime.lns.models.DiscreteLotSizing

object PSP150_3 extends App {

  new DiscreteLotSizing("data/pigment/stockingCost/PSP_150_3.txt").main(args)

}
