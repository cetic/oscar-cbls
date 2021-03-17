package oscar.anytime.lns.disabled_benchs.psp

import oscar.anytime.lns.models.DiscreteLotSizing

object PSP100c extends App {

  new DiscreteLotSizing("data/pigment/stockingCost/pigment100c.txt").main(args)

}
