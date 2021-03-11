package oscar.anytime.lns.disabled_benchs.psp

import oscar.anytime.lns.models.DiscreteLotSizing

object PSP30b extends App {

  new DiscreteLotSizing("data/pigment/stockingCost/pigment30b.txt").main(args)

}
