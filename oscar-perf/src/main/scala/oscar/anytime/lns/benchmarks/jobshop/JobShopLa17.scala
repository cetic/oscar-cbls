package oscar.anytime.lns.benchmarks.jobshop

import oscar.anytime.lns.models.JobShop

object JobShopLa17 extends App {

  new JobShop("data/jobshop/Lawrence/la17.txt", 784).main(args)

}
