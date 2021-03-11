package oscar.anytime.lns.disabled_benchs.psp

import oscar.anytime.lns.models.DiscreteLotSizing

object PSP200a extends App {

  new DiscreteLotSizing("data/pigment/stockingCost/pigment200a.txt").main(args)

}
