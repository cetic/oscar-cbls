package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.CBLSSetVar
import oscar.cbls.algo.graph._
import oscar.cbls.test.graph.RandomGraphGenerator
import oscar.cbls.test.invariants.bench.{InvBench, ToZero}

import scala.util.Random

class RevisableAStarTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val verbose = 0

  test("search from node to itself should be Distance(0)"){

    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 9
    val nbNonConditionalEdges = 5
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = Gen.oneOf(graph.nodes)

    forAll(gen){
      node =>
        val result = aStar.search(node,node,_ => true,false)
        result should be (a[Distance])
        result match{
          case Distance(_, _, distance1, _, _, _) =>
            distance1 should be (0)
        }
    }
  }

  test("search from node to unreachable node should be NeverConnected"){
    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 0, range = 0 to 0)
    val graphTemp = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val lonelyNode = new Node(nbNodes,true)

    val graph = new ConditionalGraphWithIntegerNodeCoordinates(
      coordinates = graphTemp.coordinates :+ (0,0),
      nodes = graphTemp.nodes :+ lonelyNode,
      edges = graphTemp.edges,
      nbConditions = graphTemp.nbConditions)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = Gen.oneOf(graphTemp.nodes)

    forAll(gen){
      node =>
        val result = aStar.search(node,lonelyNode,_ => true,false)
        result should be (a[NeverConnected])
    }
  }

  test("search from node to disconnected node should be NotConnected or NeverConnected "){
    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
    val graphTemp = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val lonelyNode = new Node(nbNodes,true)
    val closedEdgeToLonelyNode = new Edge(nbNonConditionalEdges,lonelyNode,graphTemp.nodes(0),50,Some(0))

    val graph = new ConditionalGraphWithIntegerNodeCoordinates(
      coordinates = graphTemp.coordinates :+ (0,0),
      nodes = graphTemp.nodes :+ lonelyNode,
      edges = graphTemp.edges :+ closedEdgeToLonelyNode,
      nbConditions = graphTemp.nbConditions+1)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => false)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = Gen.oneOf(graphTemp.nodes)


    forAll(gen){
      node =>
        val result = aStar.search(node,lonelyNode,_ => true,false)
        result should (be (a[NotConnected]) or be (a[NeverConnected]))
    }
  }

  test("Path should contain only non-conditional edges or open conditional edges"){

    val nbNodes = 10
    val nbConditionalEdges = 9
    val nbNonConditionalEdges = 5
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val openConditions = Random.shuffle(List(0,0,0,0,1,1,1,1,1))
    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = Gen.listOfN(2,Gen.oneOf(graph.nodes))

    forAll(gen){
      nodesCouple =>
        whenever(nodesCouple.size == 2 && nodesCouple.head != nodesCouple(1)){
          val nodeFrom = nodesCouple.head
          val nodeTo = nodesCouple(1)
          val result = aStar.search(nodeFrom,nodeTo,openConditions(_) == 1,includePath = true)
          result match {
            case Distance(_,_,_,_,_,path) =>
              path.get.foreach(e =>
                e.conditionID match{
                  case Some(c) => openConditions(c) should be (1)
                  case None =>
                }
              )
            case _ =>
          }
        }
    }
  }

  test("Flipping from and to on search yield the same value (and possibly the same path)"){

    val nbNodes = 60
    val nbConditionalEdges = 110
    val nbNonConditionalEdges = 50
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes / 2,
      mapSide = 1000)

    val open = Array.tabulate((nbConditionalEdges * 0.7).toInt)(_ => 1).toList
    val closed = Array.tabulate((nbConditionalEdges * 0.3).toInt)(_ => 0).toList
    val openConditions = Random.shuffle(open ::: closed)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph, _ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a: Int, b: Int) => underApproxDistanceMatrix(a)(b))

    for (nodeFrom <- graph.nodes) {
      for (nodeTo <- graph.nodes){

        val res1 = aStar.search(nodeFrom, nodeTo, openConditions(_) == 1, includePath = true)
        val res2 = aStar.search(nodeTo, nodeFrom, openConditions(_) == 1, includePath = true)
        (res1, res2) match {
          case (Distance(_, _, dist1, conditions1, _, path1), Distance(_, _, dist2, conditions2, _, path2)) =>
            dist1 should be(dist2)
            if (path1.isDefined) {

              path2 should not be None

              val pathForward = path1.get
              val pathBackward = path2.get
              val conditionsForward = pathForward.filter(_.conditionID.isDefined).sortBy(_.conditionID.get)
              val conditionsBackward = pathBackward.filter(_.conditionID.isDefined).sortBy(_.conditionID.get)
              if (pathForward.sortBy(_.id) equals pathBackward.sortBy(_.id)) {
                // If the path is the same, the conditions should be the same
                conditionsForward should be(conditionsBackward)
              }
            }

          case _ =>
        }
      }
    }
  }

  test("Opening/closing edges that are not required nor unlocking should not modify the distance between two nodes"){

    val nbNodes = 50
    val nbConditionalEdges = 90
    val nbNonConditionalEdges = 20
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val open = Array.tabulate((nbConditionalEdges * 0.5).toInt)(_ => 1L).toList
    val closed = Array.tabulate((nbConditionalEdges * 0.5).toInt)(_ => 0L).toList
    val openConditions = Random.shuffle(open ::: closed)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = for{
      n1 <- Gen.oneOf(graph.nodes)
      n2 <- Gen.oneOf(graph.nodes)
    } yield(n1,n2)

    val iterations = PosInt.from(Math.pow(graph.nodes.length,2).toInt).get

    forAll(gen,minSuccessful(iterations)){
      nodesTuple =>
        val node1 = nodesTuple._1
        val node2 = nodesTuple._2
        val result = aStar.search(node1,node2,openConditions(_) == 1)

        result match{
          case Distance(_, _, distanceBefore, required, unlocking, _) =>

            // We have a distance between node1 and node2
            // Closing or opening some random edges that are not in 'required' nor 'unlocking'
            for(i <- 0 until 20){

              val tempConditions = scrambleAllConditionsExcept(openConditions,required.toList ::: unlocking.toList)
              val tempResult = aStar.search(node1,node2,tempConditions(_) == 1)
              tempResult should be (a[Distance])
              tempResult match{
                case Distance(_, _, distanceAfter,_,_, _) =>
                  distanceAfter should be (distanceBefore) // The distance should not have changed
              }
            }

          case _ =>
        }
    }
  }

  test("Opening edges that are in distance.unlocking should reduce the distance (or keep it equal)"){
    val nbNodes = 50
    val nbConditionalEdges = 90
    val nbNonConditionalEdges = 20
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val open = Array.tabulate((nbConditionalEdges * 0.5).toInt)(_ => 1L).toList
    val closed = Array.tabulate((nbConditionalEdges * 0.5).toInt)(_ => 0L).toList
    val openConditions = Random.shuffle(open ::: closed)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = for{
      n1 <- Gen.oneOf(graph.nodes)
      n2 <- Gen.oneOf(graph.nodes)
    } yield(n1,n2)

    val iterations = PosInt.from(Math.pow(graph.nodes.length,2).toInt).get

    forAll(gen,minSuccessful(iterations)){
      nodesTuple =>
        val node1 = nodesTuple._1
        val node2 = nodesTuple._2
        val result = aStar.search(node1,node2,openConditions(_) == 1)

        result match{
          case Distance(_, _, distanceBefore, _, unlocking, _) =>

            // We have a distance between node1 and node2
            // Opening some random edges that are in 'unlocking'
            for(i <- 0 until 20){

              val tempConditions = scrambleOnlyConditionsIn(openConditions,unlocking.toList, setTo = 1)
              val tempResult = aStar.search(node1,node2,tempConditions(_) == 1)
              tempResult should be (a[Distance])
              tempResult match{
                case Distance(_, _, distanceAfter,_,_, _) =>
                  distanceAfter should be <= distanceBefore
              }
            }

          case _ =>
        }
    }
  }

  test("Closing edges that are in distance.required should augment the distance (or disconnect the node)"){
    val nbNodes = 50
    val nbConditionalEdges = 90
    val nbNonConditionalEdges = 20
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val open = Array.tabulate((nbConditionalEdges * 0.5).toInt)(_ => 1L).toList
    val closed = Array.tabulate((nbConditionalEdges * 0.5).toInt)(_ => 0L).toList
    val openConditions = Random.shuffle(open ::: closed)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = for{
      n1 <- Gen.oneOf(graph.nodes)
      n2 <- Gen.oneOf(graph.nodes)
    } yield(n1,n2)

    val iterations = PosInt.from(Math.pow(graph.nodes.length,2).toInt).get

    forAll(gen,minSuccessful(iterations)){
      nodesTuple =>
        val node1 = nodesTuple._1
        val node2 = nodesTuple._2
        val result = aStar.search(node1,node2,openConditions(_) == 1)

        result match{
          case Distance(_, _, distanceBefore, required, _, _) =>

            // We have a distance between node1 and node2
            // Closing some random edges that are in 'required'
            for(i <- 0 until 20){

              val tempConditions = scrambleOnlyConditionsIn(openConditions,required.toList, setTo = 0)
              val tempResult = aStar.search(node1,node2,tempConditions(_) == 1)
              tempResult match{
                case Distance(_, _, distanceAfter,_,_, _) =>
                  distanceAfter should be >= distanceBefore

                case _ => // The node was disconnected
              }
            }

          case _ =>
        }
    }
  }

  test("Path do respect transitive conditions"){
    for(i <- 0 until 1000){
      val nbNodes = 20
      val nbConditionalEdges = 10
      val nbNonConditionalEdges = 10
      val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
        nbConditionalEdges,
        nbNonConditionalEdges,
        nbTransitNodes = nbNodes / 2,
        mapSide = 1000)

      val open = Array.tabulate((nbConditionalEdges * 0.7).toInt)(_ => 1L).toList
      val closed = Array.tabulate((nbConditionalEdges * 0.3).toInt)(_ => 0L).toList
      val openConditions = Random.shuffle(open ::: closed)

      val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph, _ => true)
      val aStar = new RevisableAStar(graph, underApproximatingDistance = (a: Int, b: Int) => underApproxDistanceMatrix(a)(b))

      for (nodeFrom <- graph.nodes) {
        for (nodeTo <- graph.nodes){
          val res = aStar.search(nodeFrom, nodeTo, openConditions(_) == 1, includePath = true)
          res match{
            case Distance(_,_,_,_,_,path) =>
              if(path.isDefined){
                val _path = path.get.filter(e => e != path.get.head && e != path.get.last)
                _path.forall(e => e.nodeB.transitAllowed && e.nodeA.transitAllowed) should be(true)
              }

            case _ =>
          }
        }
      }
    }
  }

  def scrambleAllConditionsExcept(conditions: List[Long], except :List[Int], setTo :Int = -1): List[Long] ={

    var getNewState = () => setTo
    if(setTo == -1){
      getNewState = () => if(Random.nextDouble() > 0.5) 0 else 1
    }

    conditions.zipWithIndex.map({case (e,i) =>
      if(except.contains(i)) // Do not change the original value
        e
      else
        getNewState()
    })
  }

  def scrambleOnlyConditionsIn(conditions :List[Long], toScramble :List[Int], setTo :Int): List[Long] ={
    conditions.zipWithIndex.map({case (e,i) =>
      if(!toScramble.contains(i)) // Do not change the original value
        e
      else
      if(Random.nextDouble() > 0.5) e else setTo
    })
  }
}
