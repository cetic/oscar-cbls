package oscar.anytime.lns.disabled_benchs.xcsp.busscheduling

import oscar.anytime.lns.models.XCSP

object XCSP_BS_CNT_T2 extends App{
  new XCSP("data/xcsp3/busScheduling/BusScheduling-cnt-t2.xml", 21).main(args)
}
