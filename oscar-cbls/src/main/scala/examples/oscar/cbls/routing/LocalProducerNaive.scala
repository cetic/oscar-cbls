package examples.oscar.cbls.routing

import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.invariants.RouteLength
import oscar.cbls.business.routing.invariants.naive.BackwardNaiveRoutingConstraint
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue, FullRange, Store}
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.invariant.logic.{ConstantIntElement, Filter}
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.search.combinators.BestSlopeFirst
import oscar.visual.shapes.{VisualCircle, VisualLine}
import oscar.visual.{VisualDrawing, VisualUtil}

import java.awt.geom.Line2D
import java.awt.{BorderLayout, Color, Dimension}
import javax.swing.JFrame
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.util.Random

////////////////////////////////////////////////////////////////////////////////////////////////////
//
//      Defining the problem structure
//
////////////////////////////////////////////////////////////////////////////////////////////////////
case class Point(x : Int,
  y : Int)

case class Demand(product : Products,
  nb : Int)

case class Producer(point : Point,
  production : Array[Products]) {
  override def toString() = {
    s"Producer(point = $point,production = [${production.mkString(",")}])"
  }
}

case class Client(point : Point,
  demand : Array[Demand]) {

  override def toString() = {
    s"Client(Point = $point,\n" +
    s"\t\t       Demand = [${demand.mkString(",")}]"
  }
}

case class Vehicle(startingPoint : Point, capacity : Int)

sealed abstract class Products() {
  val size : Int
}

case class Apple() extends Products() {
  val size = 10
}

case class Pear() extends Products() {
  val size = 8
}

case class Orange() extends Products() {
  val size = 12
}

case class Strawberry() extends Products() {
  val size = 5
}

case class Problem(vehicles: Array[Vehicle], producer: Array[Producer], clients: Array[Client]) {

  def getPoints : Array[Point] = {
    vehicles.map(_.startingPoint) ++ producer.map(_.point) ++ clients.map(_.point)
  }

  def distance(from : Point,to : Point) : Long = {
    (Math.sqrt((from.x - to.x) * (from.x - to.x) + (from.y - to.y) * (from.y - to.y)) * 1000).toLong
  }

  override def toString() = {
    s"Problem(vehicles =\n\t\t${vehicles.mkString("\n\t\t")}\n" +
    s"         producer =\n\t\t${producer.mkString(",\n\t\t")}\n" +
    s"         clients =\n\t\t${clients.mkString(",\n\t\t")})"
  }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
//      Defining a problem generator
//
////////////////////////////////////////////////////////////////////////////////////////////////////

class Generator(maxX : Int,maxY : Int,seed : Option[Int] = None) {
  private val realSeed = seed match {
    case None => System.currentTimeMillis().toInt
    case Some(s) => s
  }

  private val rand = new Random(realSeed)

  private val allProducts : Array[Products] = Array(Apple(),Pear(),Orange(),Strawberry())

  private def generatePoint : Point = {
    Point(rand.nextInt(maxX),rand.nextInt(maxY))
  }

  private def generateVehicles(nbVehicle : Int) : Array[Vehicle] = {
    Array.fill(nbVehicle)(Vehicle(generatePoint,100 + rand.nextInt(5) * 10))
  }

  private def generateProducer(nbProducer : Int) : Array[Producer] = {
    Array.fill(nbProducer)(Producer(generatePoint,selectProducts))
  }
  
  private def selectProducts : Array[Products] = {
    rand.shuffle(allProducts.toList).take(1 + rand.nextInt(allProducts.length - 1)).toArray
  }

  private def generateClients(nbClients : Int) : Array[Client] = {
    Array.fill(nbClients)(Client(generatePoint,selectProducts.map(p => Demand(p,rand.nextInt(4) + 1))))
  }

  def generateProblem(nbVehicle : Int,
    nbProducer : Int,
    nbClients : Int) : Problem = 
    Problem(generateVehicles(nbVehicle),
      generateProducer(nbProducer),
      generateClients(nbClients))

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
//      Defining visualisation utilities
//
////////////////////////////////////////////////////////////////////////////////////////////////////

class MyFrame(map: SolutionMap,title: String) {
  val frame = new JFrame()
  frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
  frame.setTitle(title)
  frame.setLayout(new BorderLayout())
  frame.setPreferredSize(new Dimension(10000,10000))
  frame.add(map,BorderLayout.CENTER)
  frame.pack()
  frame.revalidate()
  frame.setVisible(true)

}

class SolutionMap(problem : Problem,v : Int,n : Int,routingIdToPoint : Int => Point,fact : Int = 10) extends VisualDrawing(false,true) {
  val points = problem.getPoints

  def coord4Drawin(x : Int): Int = x * fact

  val vehicleColors = VisualUtil.getRandomColors(v)

  val visualDepots = problem.vehicles.map(v => new VisualCircle(this,coord4Drawin(v.startingPoint.x),coord4Drawin(v.startingPoint.y),4))
  (0 until v).foreach(vehicle => visualDepots(vehicle).innerCol_=(vehicleColors(vehicle)))
  visualDepots.foreach(_.visible_=(true))

  val visualProducer = problem.producer.map(p => new VisualCircle(this,coord4Drawin(p.point.x),coord4Drawin(p.point.y),4))
  visualProducer.foreach(vp => vp.innerCol_=(Color.GREEN))
  visualProducer.foreach(_.visible_=(true))

  val visualClient = problem.clients.map(c => new VisualCircle(this,coord4Drawin(c.point.x),coord4Drawin(c.point.y),2))
  visualClient.foreach(vp => vp.innerCol_=(Color.BLUE))
  visualClient.foreach(_.visible_=(true))

  val lines = (0 until n).map(node => {
    val point = routingIdToPoint(node)
    new VisualLine(this,new Line2D.Double(coord4Drawin(point.x),coord4Drawin(point.y),coord4Drawin(point.x),coord4Drawin(point.y)))
  })

  def updateVisu(routes : IntSequence,unrouted : Iterable[Int]): Unit = {
    val expl = routes.explorerAtPosition(0).get
    unrouted.foreach(n => {
      val point = routingIdToPoint(n)
      lines(n).dest_=((coord4Drawin(point.x),coord4Drawin(point.y)))
      })
    @tailrec
    def updateVisuAux(explorer : Option[IntSequenceExplorer], prevPointId : Int, currentVehicle : Int) : Unit = {
      explorer match {
        case None =>
          val vehiclePoint = routingIdToPoint(v - 1)
          lines(prevPointId).dest_=((coord4Drawin(vehiclePoint.x),coord4Drawin(vehiclePoint.y)))
        case Some(expl) =>
          val nextPoint = routingIdToPoint(if (expl.value < v) expl.value - 1 else expl.value)
          lines(prevPointId).dest_=((coord4Drawin(nextPoint.x),coord4Drawin(nextPoint.y)))
          lines(prevPointId).outerCol_=(vehicleColors(currentVehicle))
          updateVisuAux(expl.next,expl.value,if (expl.value < v) expl.value else currentVehicle)
      }
    }
    updateVisuAux(expl.next,expl.value,expl.value)
  }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
//      Defining the capacity constraint with naive routing constraint
//
////////////////////////////////////////////////////////////////////////////////////////////////////

case class NodeState(loadPerProduct: Map[Products,Int], vehicle: Int)

object CapacityConstraint {
  def apply(routes : ChangingSeqValue,v : Int,m : Store,fonc : (Int,Int,NodeState) => NodeState) : (Array[CBLSIntVar],Array[CBLSIntVar],CapacityConstraint) = {
    val nbNodes = routes.maxValue + 1
    val load = Array.tabulate(nbNodes)(node => CBLSIntVar(m,0,FullRange,s"Total Load at node $node"))
    val vehicleOfNode = Array.tabulate(nbNodes)(node => CBLSIntVar(m,v,FullRange,s"Vehicle serving node $node"))
    val constraint = new CapacityConstraint(routes,nbNodes,v,NodeState(Map.empty,v),(0 until v).map(vehicle => NodeState(Map.empty,vehicle)).toArray,fonc,load,vehicleOfNode)
    (load,vehicleOfNode,constraint)
  }
}

class CapacityConstraint(routes : ChangingSeqValue,
                         n : Int,
                         v : Int,
                         default4Unrouted : NodeState ,
                         initValue : Array[NodeState],
                         fonc : (Int,Int,NodeState) => NodeState,
                         val loadAtEachNode : Array[CBLSIntVar],
                         val vehicleOfNode : Array[CBLSIntVar])
  extends BackwardNaiveRoutingConstraint[NodeState](routes,n,v,default4Unrouted,initValue,fonc) {

  loadAtEachNode.foreach(v => v.setDefiningInvariant(this))
  vehicleOfNode.foreach(v => v.setDefiningInvariant(this))

  override def assignNodeValue(node : Int,value : NodeState): Unit = {
    loadAtEachNode(node) := value.loadPerProduct.values.sum
    vehicleOfNode(node) := value.vehicle
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
//      Defining a problem solver
//
////////////////////////////////////////////////////////////////////////////////////////////////////

object LocalProducer extends App {
  // Generating the problem
  val gene = new Generator(100,100,Some(1000))
  val problem = gene.generateProblem(10,10,100)
  // Creating Store
  val m = Store()
  //val m = Store(checker = Some(new ErrorChecker()))
  // General variables
  val v = problem.vehicles.length
  val nbProducer = problem.producer.length
  val nbClients = problem.clients.length
  val nbProducerNodesPerProducer = nbClients / 2
  val nbProducerNodes = nbProducer * nbProducerNodesPerProducer
  val n = v + nbProducerNodes + nbClients
  // Generating the vrp
  val vrp = new VRP(m,n,v)
  val routes = vrp.routes

  // Mapping constraint between node id in routing and node id in problem
  def routingIdToClientId(id: Int): Int = {
    id - (v + nbProducerNodes)
  }

  def routingIdToProducerId(id: Int): Int = {
    (id - v) / nbClients
  }

  def routingIdToPoint(id: Int): Point = {
    if (id < v)
      problem.vehicles(id).startingPoint
    else {
      if (id < (v + nbProducerNodes))
        problem.producer(routingIdToProducerId(id)).point
      else
        problem.clients(routingIdToClientId(id)).point
    }
  }

  // The function that updates the nodes value
  def updateNodeState(from: Int, to: Int, state: NodeState): NodeState = {
    if (to < v + nbProducerNodes) {
      if (to < v)
        NodeState(state.loadPerProduct,to)
      else {
        val producer = problem.producer(routingIdToProducerId(to))
        var newLoad = state.loadPerProduct
        for (p <- producer.production) {
          newLoad = newLoad.removed(p)
        }
        NodeState(newLoad,state.vehicle)
      }
    }
    else {
      var chargeFrom = state.loadPerProduct
      problem.clients(routingIdToClientId(to)).demand.foreach(d => {
        chargeFrom.get(d.product) match {
          case None => chargeFrom = chargeFrom + {d.product -> (d.nb * d.product.size)}
          case Some(load) => chargeFrom = chargeFrom + {d.product -> ((d.nb * d.product.size) + load)}
        }
      })
      NodeState(chargeFrom,state.vehicle)
    }
  }

  // Creating the capacity constraint
  val capacityConstraint = CapacityConstraint(routes,v,m,updateNodeState)
  val loadAtEachNode : Array[CBLSIntVar] = capacityConstraint._1
  val vehicleOfNode : Array[CBLSIntVar] = capacityConstraint._2
  val maxCapacityOfNode : Array[IntValue] = Array.tabulate(n)(node => ConstantIntElement(vehicleOfNode(node),problem.vehicles.map(_.capacity.toLong)++Array(Long.MaxValue)))

  // filtering the unrouted and routed nodes between producers and clients
  val producerNodePerProducer : Array[SortedSet[Int]] = Array.tabulate(nbProducer)(p => SortedSet[Int]() ++ ((v + p * nbProducerNodesPerProducer) until (v + p * nbProducerNodesPerProducer + nbProducerNodesPerProducer)).toSet)
  val unroutedPerProducer : Array[SetValue] = producerNodePerProducer.map(s => vrp.unrouted inter CBLSSetConst(s))
  val clientSet : SortedSet[Int] = SortedSet[Int]() ++  (nbProducerNodes until n).toSet
  val unroutedClients = vrp.unrouted inter CBLSSetConst(clientSet)
  val routedClients = vrp.routed inter CBLSSetConst(clientSet)

  // Implementing the capacity constraint
  val constraints = new ConstraintSystem(m)
  (0 until n).foreach(node => constraints.add(loadAtEachNode(node) le maxCapacityOfNode(node)))
  (0 until v).foreach(vehicle => constraints.add(loadAtEachNode(vehicle) === 0))
  // Distance matrix
  val distanceMatrix : Array[Array[Long]] = Array.tabulate(n)(from => Array.tabulate(n)(to => {
    problem.distance(routingIdToPoint(from),routingIdToPoint(to))
  }))
  // Route length constraint
  val routesLength = RouteLength(routes,
    n,
    v,
    false,
    distanceMatrix,
    true)

  val kClosestPoint = Array.tabulate(n)(nodeFrom => {
    KSmallest.lazySort((0 until n).toArray,nodeTo => distanceMatrix(nodeFrom)(nodeTo))
  })

  def kClosestRoutedPoint(nodeId : Int,k : Int) = KSmallest.kFirst(k,kClosestPoint(nodeId),node => vrp.routed.value contains node)

  val clientVehicles = Array.tabulate(nbClients)(clientId => vehicleOfNode(clientId + v + nbProducerNodes))
  val unservedClients = Filter(clientVehicles,_ == v)
  val penalty = distanceMatrix.map(_.max).max * nbProducer * 2 + 10
  val penalty4UnservedClients = Cardinality(unservedClients) * penalty
  // This penalty is here to prevent to have many producer nodes routed in the same time
  val routedNodesPenalty = Cardinality(vrp.routed) * 10
  // Defining the objectives
  val obj = new CascadingObjective(constraints,Objective(routesLength(0) + penalty4UnservedClients + routedNodesPenalty))

  m.close()

  val map = new SolutionMap(problem,v,n,routingIdToPoint)
  // change visu to false to disable the visualization
  val visu = true
  if (visu) {
    val window = new MyFrame(map,"test")

    map.updateVisu(vrp.routes.value,vrp.unrouted.value)

  }

  // The producer are represented by many nodes. This function gets one unrouted node for a given producer if there is one
  def producerNodeToInsert(producerId : Int) : List[Int] = {
    val unroutedProducerNode = unroutedPerProducer(producerId).value
    if (unroutedProducerNode.isEmpty)
      Nil
    else
      List(unroutedProducerNode.min)
  }

  // Insert a producer with a the given id
  def insertProducerId(producerId : Int,prevNode : Option[Int] = None) = {
    InsertPointUnroutedFirst(() => producerNodeToInsert(producerId),
      () => _ => prevNode match {case None => vrp.routed.value; case Some(p) => List(p)},
      vrp)
  }

  // Insert one node of all producer
  def mkInsertAllProducer(producerList : List[Int],prevPos : Option[Int] = None,insert : Boolean = true) : Neighborhood = {
    producerList match {
      case Nil =>
        if (insert)
          InsertPointUnroutedFirst(() => unroutedClients.value,() => (_ : Int) => prevPos match {case None => vrp.routed.value;case Some(p) => List(p)},vrp)
        else
          OnePointMove(() => routedClients.value,() => (_ : Int) => prevPos match {case None => vrp.routed.value;case Some(p) => List(p)},vrp)
      case h1 :: t =>
        insertProducerId(h1,prevPos) dynAndThen ((m : InsertPointMove) => mkInsertAllProducer(t,Some(m.insertedPoint)))
    }
  }

  // Insert all the producers and one client
  val insertAllProducerAndClient = mkInsertAllProducer((0 until nbProducer).toList) name "InsertAllProducerAndClient"

  // Insert all the producers and move one client
  val insertAllProducerAndMoveClient = mkInsertAllProducer((0 until nbProducer).toList,None,false) name "InsertAllProducerAndMoveClients"

  // Insert one point
  def insertPoint = InsertPointUnroutedFirst(() => unroutedClients.value,
    () => (_ : Int) => vrp.routed.value,
    vrp)

  // Insert one producer and one client
  def insertProducerAndClient = InsertPointUnroutedFirst(() => {
    unroutedPerProducer.map(s => {
      if (s.value.isEmpty) None else Some(s.value.min)
    }).flatten
  },
    () => (insertedPoint : Int) => kClosestRoutedPoint(insertedPoint,10),
    vrp) andThen InsertPointUnroutedFirst(() => unroutedClients.value,
      () => (_ : Int) => vrp.routed.value,
      vrp) name "InsertProducerAndClient"

  // Move One point
  def onePointMove = OnePointMove(() => vrp.routed.value,
    () => (_ : Int) => vrp.routed.value,
    vrp)

  // ThreeOpt
  def threeOpt(k : Int) = ThreeOpt(() => vrp.routed.value,
    () => (fstPoint : Int) => kClosestRoutedPoint(fstPoint,k),
    vrp)

  // Remove one point (sometimes to many points are inserted
  def removePoint = RemovePoint(() => v until v + nbProducerNodes,
    vrp)

  // Defining the search strategy
  val searchStrategy = BestSlopeFirst(List(insertPoint,
    insertProducerAndClient,
    insertAllProducerAndClient,
    insertAllProducerAndMoveClient,
    onePointMove,
    removePoint,
    threeOpt(3) name "ThreeOpt3",
    threeOpt(10) name "ThreeOpt10").map(n => profile(n)))

  // Adding visualisation
  val search = if (visu) searchStrategy afterMove  map.updateVisu(vrp.routes.value,vrp.unrouted.value) else searchStrategy

  // Do the search !
  search.verbose = 1
  search.doAllMoves(obj = obj)
  map.updateVisu(vrp.routes.value,vrp.unrouted.value)
  println(search.profilingStatistics)
  println(s"Unserved clients : ${unservedClients} (${penalty})")

  // Utilities function to pretty print the solution

  private def treateNode(nodeValue : Int,load : Map[Products,Int],currentCapacity : Int) : (String,Map[Products,Int]) = {
    if (nodeValue < v) {
      val currentLoad = load.values.sum
      ((s"======== VEHICLE ${nodeValue} ======== (current load : $currentLoad ( < ${problem.vehicles(nodeValue).capacity}) -- (${load.mkString(",")}))"),Map[Products,Int]())
    }
    else {
      if (nodeValue < v + nbProducerNodes) {
        var newLoad = load
        var prodToCharge = Map[Products,Int]()
        val producer : Producer = problem.producer(routingIdToProducerId(nodeValue))
        producer.production.foreach(p => {
          load.get(p) match {
            case None =>
            case Some(q) => prodToCharge += {p-> q}
              newLoad = newLoad.removed(p)
          }
        })
        (s"Node $nodeValue (Producer ${routingIdToProducerId(nodeValue)}) : Loading (${prodToCharge.mkString(",")}) --  Current Load : ${load.map(_._2).sum} (< ${currentCapacity}) -- Load After : ${load.mkString(",")}",newLoad)
      }
      else {
        var newLoad = load
        val client = problem.clients(routingIdToClientId(nodeValue))
        client.demand.foreach(pAndQ => {
          val newQ = pAndQ.nb * pAndQ.product.size + (load.get(pAndQ.product) match {
            case None => 0
            case Some(q) => q
          })
          newLoad += {pAndQ.product -> newQ}
        })
        (s"Node $nodeValue (Client) : Unloading (${client.demand.map(pAndQ => (pAndQ.product,pAndQ.nb * pAndQ.product.size)).mkString(",")} -- Current Load : ${load.map(_._2).sum} (< ${currentCapacity}) -- (Load After : ${load.mkString(",")})",newLoad)
      }
    }
  }

  def printResult = {
    def printResultAux(expl : Option[IntSequenceExplorer],load : Map[Products,Int],currentCapacity : Int) : List[String] = {
      expl match {
        case None => List()
        case Some(e) =>
          val nextCapa = if (e.value < v) problem.vehicles((e.value - 1) max 0).capacity else currentCapacity
          val nodeTreate = treateNode(e.value,load,currentCapacity)
          nodeTreate._1:: printResultAux(e.prev,nodeTreate._2,nextCapa)
      }
    }

    @tailrec
    def getLastExplAndCapa(expl : Option[IntSequenceExplorer], current : IntSequenceExplorer, currentCapa : Int) : (IntSequenceExplorer,Int) = {
      expl match {
        case None => (current,currentCapa)
        case Some(e) =>
          val newCapa = if (e.value < v) problem.vehicles(e.value).capacity else currentCapa
          getLastExplAndCapa(e.next,e,currentCapa)
      }
    }
    val fstExpl = routes.value.explorerAtPosition(0).get
    val (lastExpl,lastCapa) = getLastExplAndCapa(fstExpl.next,fstExpl,0)
    println(printResultAux(Some(lastExpl),Map[Products,Int](),lastCapa).reverse.mkString("\n"))
  }

  printResult
}
