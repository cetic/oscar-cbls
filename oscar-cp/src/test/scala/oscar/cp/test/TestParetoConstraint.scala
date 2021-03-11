/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp.testUtils.TestSuite
import oscar.cp._
import oscar.cp.constraints.MinAssignment
import oscar.util.selectMin
import oscar.cp.multiobjective.Pareto
import oscar.cp.multiobjective.ListPareto
import oscar.cp.constraints.ParetoConstraint
import oscar.cp.core.CPSol

class TestParetoConstraint extends TestSuite {

  val coord1 = Array((985, 588), (847, 313), (254, 904), (434, 606), (978, 748), (569, 473), (317, 263), (562, 234), (592, 262), (596, 189))
  val coord2 = Array((108, 372), (40, 67), (389, 350), (606, 719), (847, 68), (94, 86), (434, 614), (514, 416), (67, 399), (530, 231))
  val coord3 = Array((734, 660), (210, 581), (128, 202), (549, 564), (459, 961), (585, 882), (277, 614), (981, 806), (576, 137), (886, 99))
  val coords = Array(coord1, coord2, coord3)

  // 1:min, 2:min
  val sols1 = Array((3984, 2895), (4835, 2781), (4178, 2838), (2828, 3543), (3238, 3304), (3476, 3020), (3000, 3536), (4071, 2860), (3291, 3265), (2711, 4745), (4513, 2803), (4758, 2787), (3918, 2963), (2779, 4600), (2817, 4547), (4049, 2883), (3417, 3194), (4343, 2818))
  // 1:min, 3:min
  val sols2 = Array((5270, 3241), (4625, 3328), (3774, 3699), (3207, 4224), (3073, 4531), (3102, 4410), (2711, 5379), (3063, 4583), (3308, 3832), (2779, 4730), (2922, 4707), (3232, 4011), (3297, 3875), (3133, 4245), (3623, 3710), (3932, 3494), (3981, 3382), (4830, 3297), (3516, 3770))
  // 2:min, 3:min
  val sols3 = Array((5423, 3241), (4471, 3328), (3521, 4212), (2781, 5983), (2799, 5754), (3352, 4335), (2959, 4711), (2826, 5044), (3118, 4538), (4166, 3660), (2928, 4975), (2808, 5289), (3882, 3976), (2803, 5396), (3166, 4374), (3655, 3997), (3546, 4057), (2783, 5924), (2797, 5813), (2934, 4866), (3731, 3992), (3141, 4529), (4072, 3813), (3890, 3819), (4427, 3542), (5395, 3297), (4458, 3387))

  // 1:max, 2:max
  val sols4 = Array((5665, 5474), (5652, 5490), (5536, 5781), (5389, 5790), (4508, 5893), (4163, 5894), (4793, 5808), (4662, 5834), (5633, 5679), (4655, 5884), (5039, 5806), (5776, 5471))
  // 1:max, 3:max
  val sols5 = Array((5760, 5900), (5762, 5541), (5775, 5132), (5752, 5919), (5463, 6788), (5212, 6938), (5363, 6927), (5564, 6427), (5488, 6632), (5166, 7010), (5067, 7063), (5368, 6875), (5483, 6684), (5079, 7023), (5630, 6332), (5719, 6011), (5713, 6229), (5776, 4966))
  // 2:max, 3:max
  val sols6 = Array((5863, 6120), (5740, 6523), (5522, 6770), (5640, 6760), (5007, 7063), (5894, 4912), (5746, 6496), (5048, 6943), (5664, 6569), (5849, 6215), (5885, 5676), (5893, 5148), (5198, 6875), (5182, 6922), (5848, 6451), (5039, 6971), (5395, 6874))

  // 1:min, 2:max
  val sols7 = Array((3238, 5709), (2774, 5443), (3266, 5855), (3245, 5746), (2711, 4745), (2716, 5190), (2975, 5682), (4163, 5894), (3536, 5884), (4136, 5885))
  // 1:min, 3:max
  val sols8 = Array((4583, 7061), (3227, 6679), (3462, 6794), (4421, 7049), (3618, 6832), (2981, 6604), (2816, 6526), (2716, 5703), (2711, 5379), (2721, 5917), (2727, 5937), (3392, 6793), (3964, 6999), (3898, 6868), (3984, 7035), (5067, 7063))
  // 2:min, 3:max
  val sols9 = Array((5007, 7063), (3982, 6995), (4006, 7032), (2781, 5983), (2787, 6622), (2826, 6783), (3068, 6796), (3219, 6936), (3688, 6938), (3195, 6891), (4538, 7061), (3999, 7031), (3808, 6994), (4057, 7046))

  // 1:max, 2:min
  val sols10 = Array((5775, 4747), (5774, 4739), (5751, 3676), (5713, 3373), (5510, 2817), (5255, 2783), (5290, 2799), (4835, 2781), (5516, 3103), (5767, 4081), (5738, 3438), (5759, 3829), (5698, 3268), (5776, 5471))
  // 1:max, 3:min
  val sols11 = Array((5655, 4197), (5582, 3730), (5318, 3617), (5686, 4390), (5748, 4879), (5619, 3983), (5597, 3872), (5741, 4827), (5745, 4846), (5730, 4704), (5688, 4696), (5776, 4966), (5775, 4938), (5630, 4106), (5337, 3717), (5305, 3392), (5671, 4248), (5270, 3241))
  // 2:max, 3:min
  val sols12 = Array((5885, 4179), (5698, 3874), (5458, 3444), (5609, 3546), (5584, 3494), (5894, 4912), (5676, 3612), (5423, 3241), (5704, 3875), (5780, 3990))

  // 1:min, 2:min, 3:min
  val sols13 = Array((5066, 4427, 3542), (4954, 4441, 3609), (3516, 5429, 3770), (3564, 5348, 3774), (4625, 4471, 3328), (4723, 4458, 3387), (4029, 5023, 3386), (3774, 5558, 3699), (4576, 4951, 3440), (3932, 5584, 3494), (4028, 4563, 3542), (4863, 3352, 4335), (3765, 3731, 4373), (3877, 4242, 3881), (4911, 2959, 4711), (4459, 3166, 4374), (5006, 3521, 4212), (3507, 4537, 4084), (3558, 4693, 3846), (4190, 3997, 4022), (4646, 3731, 3992), (3623, 4326, 3710), (4956, 3141, 4529), (3899, 4100, 4049), (4265, 3562, 4562), (3457, 4157, 4401), (3446, 4182, 4338), (4791, 4166, 3660), (5231, 3882, 3976), (4421, 3317, 4476), (3525, 3807, 4509), (3073, 4685, 4531), (4258, 3379, 4763), (3984, 2895, 6223), (3004, 4115, 4957), (4343, 2818, 6343), (3867, 3401, 4990), (4032, 3139, 5068), (4195, 3686, 4106), (4783, 3072, 4801), (3902, 3175, 4769), (3394, 3462, 5890), (3417, 3194, 5854), (3133, 4999, 4245), (3460, 3232, 5121), (3451, 3871, 4422), (3827, 3183, 5624), (4097, 3699, 4047), (3102, 5061, 4410), (4500, 4009, 3976), (4491, 2826, 5044), (4178, 2838, 6214), (3270, 3486, 5227), (4049, 2883, 5396), (3343, 3913, 4682), (2931, 3765, 5319), (3232, 5074, 4011), (4555, 3067, 4957), (3103, 3758, 4886), (3033, 4479, 4804), (2779, 4600, 4730), (3918, 2963, 5714), (3356, 5075, 3836), (3849, 4301, 4038), (3370, 4672, 4170), (3319, 4684, 4227), (4099, 3422, 4563), (5255, 2783, 5924), (4456, 4052, 4019), (4758, 2787, 6622), (3116, 3747, 4960), (3320, 3745, 4936), (2875, 4453, 5009), (4513, 2803, 5396), (2711, 4745, 5379), (3589, 3187, 5421), (3291, 3265, 5952), (3806, 3115, 5721), (4071, 2860, 5748), (3201, 4072, 4844), (3063, 4517, 4583), (4031, 3130, 5069), (4554, 3135, 4638), (2841, 4116, 5068), (3000, 3536, 5146), (3476, 3020, 6066), (3469, 4679, 4072), (4051, 3815, 4187), (4660, 3408, 4354), (4592, 3116, 4860), (5290, 2799, 5754), (3203, 4473, 4413), (3308, 5156, 3832), (3306, 4695, 4153), (3153, 4138, 4569), (3283, 4059, 4716), (3442, 3479, 4794), (4271, 4072, 3813), (4369, 4059, 3872), (5006, 2928, 4975), (2817, 4547, 6222), (3238, 3304, 6113), (3459, 3464, 5546), (2922, 5203, 4707), (3151, 4581, 4298), (2828, 3543, 5579), (3297, 4707, 3875), (3991, 4142, 3876), (4059, 3850, 4149), (3349, 3679, 5130), (3435, 3466, 5356), (3551, 4623, 4046), (3452, 4548, 4280), (4026, 3151, 5432), (4253, 3024, 5347), (4219, 4309, 3839), (2883, 3974, 5070), (2986, 4196, 4810), (3283, 4016, 4818), (3413, 4022, 4524), (3096, 4150, 4807), (3715, 3141, 5643), (3563, 3454, 4861), (4379, 2978, 5051), (3967, 3548, 4611), (3859, 3137, 5502), (4128, 3079, 5124), (4528, 3121, 4872), (4529, 3129, 4706), (4559, 3118, 4538), (4835, 2781, 5983), (4870, 2797, 5813), (4067, 3155, 4898), (3776, 3706, 4436), (4751, 3504, 4342), (4426, 2838, 5871), (3380, 4334, 4348), (3731, 3054, 5805), (3408, 3630, 4559), (4810, 3015, 4952), (5262, 2808, 5289), (3477, 4200, 4133), (4405, 3505, 4425), (3343, 3960, 4598), (3796, 3878, 4309), (4037, 3395, 4599), (3671, 3933, 4086), (5408, 2934, 4866), (4625, 4268, 3715), (3531, 4024, 4172), (4510, 4466, 3523), (4874, 3898, 3955), (4407, 4244, 3783), (4347, 3548, 4362), (3850, 3573, 4207), (4901, 3655, 3997), (3121, 4604, 4535), (3288, 4032, 4810), (4509, 3546, 4057), (4725, 3974, 3926), (3207, 5052, 4224), (3963, 4930, 3678), (4255, 3810, 4039), (3980, 5503, 3498), (3979, 4643, 3703), (3981, 5104, 3382), (4830, 5395, 3297), (4372, 4992, 3541), (5270, 5423, 3241), (5087, 3890, 3819))

  def equalsSol(sol1: Iterable[(Int, Int)], sol2: Iterable[(Int, Int)]): Boolean = {
    if (sol1.size != sol2.size) false
    else sol1.forall(s1 => sol2.exists(s2 => s2 equals s1))
  }

  def equals3Sol(sol1: Iterable[(Int, Int, Int)], sol2: Iterable[(Int, Int, Int)]): Boolean = {
    if (sol1.size != sol2.size) false
    else sol1.forall(s1 => sol2.exists(s2 => s2 equals s1))
  }

  def computeDistMatrix(coord: IndexedSeq[(Int, Int)]): Array[Array[Int]] = {
    Array.tabulate(coord.size, coord.size)((i, j) => getDist(coord(i), coord(j)))
  }

  def getDist(p1: (Int, Int), p2: (Int, Int)): Int = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.round(math.sqrt(dx * dx + dy * dy)).toInt
  }

  def solveMoTSP(objectives: (Int, Boolean)*): List[IndexedSeq[Int]] = {

    val nObjs = objectives.size
    val Objs = 0 until nObjs

    val problems = objectives.map(_._1).toArray
    val isMax = objectives.map(_._2).toArray

    val paretoSet: Pareto[CPSol] = ListPareto(isMax: _*)

    val nCities = 10
    val Cities = 0 until nCities
    val distMatrices = for (p <- problems) yield computeDistMatrix(coords(p))

    val cp = CPSolver()
    val succ = Array.fill(nCities)(CPIntVar(Cities)(cp))
    val totDists = for (o <- Objs) yield CPIntVar(0, distMatrices(o).flatten.sum)(cp)

    cp.post(circuit(succ), Strong)
    for (o <- Objs) cp.add(sum(Cities)(i => distMatrices(o)(i)(succ(i))) === totDists(o))
    cp.post(new ParetoConstraint(paretoSet, isMax, totDists.toArray))

    cp.search {
      selectMin(Cities)(!succ(_).isBound)(succ(_).size) match {
        case None => noAlternative
        case Some(i) => {
          val j = if (isMax(0)) selectMin(Cities)(succ(i).hasValue(_))(-distMatrices(0)(i)(_)).get
          else selectMin(Cities)(succ(i).hasValue(_))(distMatrices(0)(i)(_)).get
          branch(cp.post(succ(i) === j))(cp.post(succ(i) !== j))
        }
      }
    } onSolution {
      val inserted = paretoSet.insert(cp.lastSol, totDists.map(_.value): _*)
      // Only non-dominated solutions are found
      inserted should be(true)
    }
    cp.start
    paretoSet.objectiveSols
  }

  test("Completude on the bi-TSP 1:min 2:min") {
    val sols = solveMoTSP((0, false), (1, false)).map(s => (s(0), s(1)))
    equalsSol(sols, sols1) should be(true)
  }

  test("Completude on the bi-TSP 1:min 3:min") {
    val sols = solveMoTSP((0, false), (2, false)).map(s => (s(0), s(1)))
    equalsSol(sols, sols2) should be(true)
  }

  test("Completude on the bi-TSP 2:min 3:min") {
    val sols = solveMoTSP((1, false), (2, false)).map(s => (s(0), s(1)))
    equalsSol(sols, sols3) should be(true)
  }

  test("Completude on the bi-TSP 1:max 2:max") {
    val sols = solveMoTSP((0, true), (1, true)).map(s => (s(0), s(1)))
    equalsSol(sols, sols4) should be(true)
  }

  test("Completude on the bi-TSP 1:max 3:max") {
    val sols = solveMoTSP((0, true), (2, true)).map(s => (s(0), s(1)))
    equalsSol(sols, sols5) should be(true)
  }

  test("Completude on the bi-TSP 2:max 3:max") {
    val sols = solveMoTSP((1, true), (2, true)).map(s => (s(0), s(1)))
    equalsSol(sols, sols6) should be(true)
  }

  test("Completude on the bi-TSP 1:min 2:max") {
    val sols = solveMoTSP((0, false), (1, true)).map(s => (s(0), s(1)))
    equalsSol(sols, sols7) should be(true)
  }

  test("Completude on the bi-TSP 1:min 3:max") {
    val sols = solveMoTSP((0, false), (2, true)).map(s => (s(0), s(1)))
    equalsSol(sols, sols8) should be(true)
  }

  test("Completude on the bi-TSP 2:min 3:max") {
    val sols = solveMoTSP((1, false), (2, true)).map(s => (s(0), s(1)))
    equalsSol(sols, sols9) should be(true)
  }

  test("Completude on the bi-TSP 1:max 2:min") {
    val sols = solveMoTSP((0, true), (1, false)).map(s => (s(0), s(1)))
    equalsSol(sols, sols10) should be(true)
  }

  test("Completude on the bi-TSP 1:max 3:min") {
    val sols = solveMoTSP((0, true), (2, false)).map(s => (s(0), s(1)))
    equalsSol(sols, sols11) should be(true)
  }

  test("Completude on the bi-TSP 2:max 3:min") {
    val sols = solveMoTSP((1, true), (2, false)).map(s => (s(0), s(1)))
    equalsSol(sols, sols12) should be(true)
  }

  test("Completude on the tri-TSP 1:min 2:min 3:min") {
    val sols = solveMoTSP((0, false), (1, false), (2, false)).map(s => (s(0), s(1), s(2)))
    equals3Sol(sols, sols13) should be(true)
  }
}
