package oscar.anytime.lns.disabled_benchs.xcsp.busscheduling

import oscar.anytime.lns.models.XCSP

object XCSP_BS_CNT_R1a extends App{
  new XCSP("data/xcsp3/busScheduling/BusScheduling-cnt-r1a.xml", 13).main(args)
}
