package oscar.cbls.test.routing

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls._
import oscar.cbls.benchmarks.vrp.RoutingMatrixGenerator
import oscar.cbls.business.routing.invariants._
import oscar.cbls.business.routing.invariants.capa.{ForwardCumulativeConstraintOnVehicle, ForwardCumulativeIntegerDimensionOnVehicle}
import oscar.cbls.test.invariants.bench._

class RoutingTestSuite extends FunSuite with Checkers{

  val verbose = 0

  test("Node of vehicle"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    NodesOfVehicle(route,v)
    bench.run()
  }

  test("Constant routing distance - global distance - symmetric"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n-1,v)
    val distanceMatrix = RoutingMatrixGenerator(n)._1
    RouteLength(route,n,v,false,distanceMatrix,true)
    bench.run()
  }

  test("Constant routing distance - per vehicle distance - symetric"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n-1,v)
    val distanceMatrix = RoutingMatrixGenerator(n)._1
    RouteLength(route,n,v,true,distanceMatrix,true)
    bench.run()
  }

  test("NodeVehicleRestrictions"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    val nodeVehicleRestrictions = RoutingMatrixGenerator.generateRestrictions(n,v,n/v)
    NodeVehicleRestrictions(route,v,nodeVehicleRestrictions)
    bench.run()
  }

  test("RouteSuccessorsAndPredecessors"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    val defaultWhenNotInSeq = -1
    RouteSuccessorAndPredecessors(route,v,defaultWhenNotInSeq)
    bench.run()
  }

  test("VehicleOfNodes"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    VehicleOfNodes(route,v)
    bench.run()
  }


  test("GenericCumulativeIntegerDimensionOnVehicle"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    val limite = 10

    def genMatrix(node:Int):Array[Array[Int]] = {
      val init = Int.MinValue
      val matrix: Array[Array[Int]] = Array.tabulate(n,n)((n1:Int,n2:Int)=>init)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          matrix(elt)(elt1) =  if(elt==elt1) 0 else scala.util.Random.nextInt(limite)
        }
      }
      matrix
    }
    val matrix = genMatrix(n)
    def genOperation(node:Int):Array[Array[Int]] = {
      val oper: Array[Array[Int]] = Array.ofDim(n,n)
      var t1:Array[Int] =Array.ofDim(n)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          oper(elt)(elt1) =  if(matrix(elt)(elt1)==0) scala.util.Random.nextInt(3) else scala.util.Random.nextInt(4)
        }
      }
      oper
    }

    val oper = genOperation(n)
    def op(n1:Long,n2:Long,c:Long): Long= {
      oper(n1)(n2) match {
        case 0 => c + matrix(n1)(n2)
        case 1 => 0L max c - matrix(n1)(n2)
        case 2 => c * matrix(n1)(n2)
        case 3 => c % matrix(n1)(n2)
      }
    }

    def start() : Array[IntValue]= { Array.tabulate(v)((car:Int)=> scala.util.Random.nextInt(limite))}
    val  s = start()
    val inv = ForwardCumulativeIntegerDimensionOnVehicle(routes=route,
      n=n,
      v=v,
      op=op,
      contentAtStart = s,
      defaultForUnroutedNodes = 0,
      minContent = 0,
      maxContent = Int.MaxValue - 2
    )

    bench.run()
  }

  test("ForwardCumulativeConstraintOnVehicle"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)


    val limite = 10
    def genMatrix(node:Int):Array[Array[Int]] = {
      val init = Int.MinValue
      val matrix: Array[Array[Int]] = Array.tabulate(n,n)((n1:Int,n2:Int)=>init)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          matrix(elt)(elt1) =  if(elt==elt1) 0 else scala.util.Random.nextInt(limite)
        }
      }
      matrix
    }
    val matrix = genMatrix(n)
    def genOperation(node:Int):Array[Array[Int]] = {
      val oper: Array[Array[Int]] = Array.ofDim(n, n)
      var t1:Array[Int] =Array.ofDim(n)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          oper(elt)(elt1) =  if(matrix(elt)(elt1)==0) scala.util.Random.nextInt(3) else scala.util.Random.nextInt(4)
        }
      }
      oper
    }

    val oper = genOperation(n)
    def op(n1:Long,n2:Long,c:Long): Int= {
      oper(n1)(n2) match {
        case 0 => c + matrix(n1)(n2)
        case 1 => 0L max c - matrix(n1)(n2)
        case 2 => c * matrix(n1)(n2)
        case 3 => c % matrix(n1)(n2)
      }
    }

    def start() : Array[Long]= { Array.tabulate(v)((car:Int)=> scala.util.Random.nextInt(limite))}
    val  s = start()

    val inv = ForwardCumulativeConstraintOnVehicle(route,n,v,op,limite,s,
      maxCheckpointLevel = 2,
      capacityName = "test capacity"
    )


    val go = System.nanoTime()
    bench.run()
    println("GenericCumulativeConstraint(n ="+n+" v ="+v+") : "+((System.nanoTime()-go)/Math.pow(10,9)) + "s")
  }

  test("GenericCumulativeIntegerDimensionOnVehicleWithVar"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(), Shuffle()))
    val n = 100
    val v = 5
    val route = bench.genRouteOfNodes(n,v)
    val contentAtStart = bench.genIntVarsArray(v)

    val limite = 10
    def genMatrix(node:Int):Array[Array[Int]] = {
      val init = Int.MinValue
      val matrix: Array[Array[Int]] = Array.tabulate(n,n)((n1:Int,n2:Int)=>init)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          matrix(elt)(elt1) =  if(elt==elt1) 0 else scala.util.Random.nextInt(limite)
        }
      }
      matrix
    }
    val matrix = genMatrix(n)
    def genOperation(node:Int):Array[Array[Int]] = {
      val oper: Array[Array[Int]] = Array.ofDim(n,n)
      var t1:Array[Int] =Array.ofDim(n)
      for (elt <- 0 until node) {
        for (elt1 <- 0 until node) {
          oper(elt)(elt1) =  if(matrix(elt)(elt1)==0) scala.util.Random.nextInt(3) else scala.util.Random.nextInt(4)
        }
      }
      oper
    }

    val oper = genOperation(n)
    def op(n1:Long,n2:Long,c:Long): Int= {
      oper(n1)(n2) match {
        case 0 => c + matrix(n1)(n2)
        case 1 => 0L max c - matrix(n1)(n2)
        case 2 => c * matrix(n1)(n2)
        case 3 => c % matrix(n1)(n2)
      }
    }

    def start() : Array[CBLSIntVar]= { Array.tabulate(v)((car:Int)=> CBLSIntVar(route.model,op(car,car,0)))}
    val  s = start()

    var inv = ForwardCumulativeIntegerDimensionOnVehicle(route,n,v,op,contentAtStart,defaultForUnroutedNodes= -1,maxContent = Int.MaxValue,minContent = Int.MinValue)

    val go = System.nanoTime()
    bench.run()
    print("GenericCumulativeIntegerDimensionOnVehicleWithVar(n ="+n+" v ="+v+") : "+((System.nanoTime()-go)/Math.pow(10,9))+" s")
  }


  test("Star exploration"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), Shuffle(), MultipleMove()))
    val n = 100
    val v = 4
    val route = bench.genRouteOfNodesForCheckPoint(n-1,v)
    val distanceMatrix = RoutingMatrixGenerator(n)._1
    RouteLength(route,n,v,true,distanceMatrix,true)
    bench.run()
  }
}
