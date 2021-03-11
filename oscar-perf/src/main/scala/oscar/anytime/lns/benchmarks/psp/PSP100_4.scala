package oscar.anytime.lns.benchmarks.psp

import oscar.anytime.lns.models.DiscreteLotSizing

object PSP100_4 extends App {

  new DiscreteLotSizing("data/pigment/stockingCost/PSP_100_4.txt").main(args)

}
