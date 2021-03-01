package oscar.anytime.lns.benchmarks.xcsp.warehouse

import oscar.anytime.lns.models.XCSP

object XCSP_Warehouse_cap101 extends App{
  new XCSP("data/xcsp3/warehouse/Warehouse-cap101.xml", 804126).main(args)
}
