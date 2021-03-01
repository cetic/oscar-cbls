package oscar.anytime.lns.disabled_benchs.xcsp.busscheduling

import oscar.anytime.lns.models.XCSP

object XCSP_BS_CNT_R4 extends App{
  new XCSP("data/xcsp3/busScheduling/BusScheduling-cnt-r4.xml", 28).main(args)
}
