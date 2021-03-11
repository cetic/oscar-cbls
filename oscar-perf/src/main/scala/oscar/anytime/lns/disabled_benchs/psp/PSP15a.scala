package oscar.anytime.lns.disabled_benchs.psp

import oscar.anytime.lns.models.{DiscreteLotSizing, ProductMatrixTSP}

object PSP15a extends App {

  new DiscreteLotSizing("data/pigment/stockingCost/pigment15a.txt").main(args)

}
