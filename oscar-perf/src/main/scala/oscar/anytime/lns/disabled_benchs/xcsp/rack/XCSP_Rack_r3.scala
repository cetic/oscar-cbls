package oscar.anytime.lns.disabled_benchs.xcsp.rack

import oscar.anytime.lns.models.XCSP

object XCSP_Rack_r3 extends App{
  new XCSP("data/xcsp3/rack/Rack-r3.xml", 1200).main(args)
}
