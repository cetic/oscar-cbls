package oscar.anytime.lns.disabled_benchs.psp

import oscar.anytime.lns.models.DiscreteLotSizing

object PSP200_2 extends App {

  new DiscreteLotSizing("data/pigment/stockingCost/PSP_200_2.txt").main(args)

}
