package oscar.cbls.test.core.propagation

import oscar.cbls.core.propagation.{
  TestInvariantElement,
  TestPropagationElement,
  TestPropagationStructure,
  TestVariableElement
}
import scala.annotation.tailrec

object TestPropagation extends App {

  val rand = new scala.util.Random(1000)

  val tempStruct = new TestPropagationStructure

  val maxElemPerLayer = 20
  val minElemPerLayer = 3
  val minInputVar     = 3
  val maxInputVar     = 10
  val minOutputVar    = 3
  val maxOutputVar    = 10

  def generateInputVariables(min: Int, max: Int): List[TestVariableElement] = {
    @tailrec
    def generateList(
      nb: Int = min + rand.nextInt(max - min),
      res: List[TestVariableElement] = List()
    ): List[TestVariableElement] = {
      if (nb == 0) {
        res
      } else {
        val newVar = new TestVariableElement(tempStruct)
        newVar.theoreticalLayer = 0
        generateList(nb - 1, newVar :: res)
      }
    }
    generateList()
  }

  def generateInvariant(
    min: Int,
    max: Int,
    pool: List[TestVariableElement]
  ): (List[TestInvariantElement], List[TestVariableElement]) = {
    def generateList(
      nb: Int = min + rand.nextInt(max - min),
      invariants: List[TestInvariantElement] = List(),
      outVar: List[TestVariableElement] = List()
    ): (List[TestInvariantElement], List[TestVariableElement]) = {
      if (nb == 0) {
        (invariants, outVar)
      } else {
        val newInvariant = new TestInvariantElement(tempStruct)
        val input = rand.shuffle(pool).take(minInputVar + rand.nextInt(maxInputVar - minInputVar))
        newInvariant.theoreticalLayer = input.map(_.theoreticalLayer).max + 1
        input.foreach(v => newInvariant.registerStaticAndDynamicDependency(v))
        val output = List.fill(minOutputVar + rand.nextInt(maxOutputVar - minOutputVar))(
          new TestVariableElement(tempStruct)
        )
        output.foreach(v => {
          v.setDefiningInvariant(newInvariant)
          v.theoreticalLayer = newInvariant.theoreticalLayer + 1
        })
        generateList(nb - 1, newInvariant :: invariants, output ::: outVar)
      }
    }
    generateList()
  }

  def generateStructure(nbLayer: Int): List[TestPropagationElement] = {
    val input = generateInputVariables(minElemPerLayer, maxElemPerLayer)

    @tailrec
    def insertElem(
      e: TestPropagationElement,
      l: List[TestPropagationElement],
      res: List[TestPropagationElement] = List()
    ): List[TestPropagationElement] = {
      l match {
        case Nil    => e :: res
        case h :: t => if (h == e) insertElem(e, t, res) else insertElem(e, t, h :: res)
      }
    }

    def generateLayers(
      pool: List[TestVariableElement] = input,
      currentMaxLayer: Int = 0,
      elements: List[TestPropagationElement] = List()
    ): List[TestPropagationElement] = {
      if (currentMaxLayer >= nbLayer) {
        elements
      } else {
        val invAndVar = generateInvariant(minElemPerLayer, maxElemPerLayer, pool)
        val newPool   = invAndVar._2 ::: pool
        val maxLayer  = newPool.map(_.theoreticalLayer).max
        val newElems =
          (invAndVar._1 ::: newPool).foldLeft(elements)((elems, e) => insertElem(e, elems))
        generateLayers(newPool, maxLayer, newElems)
      }
    }
    generateLayers()

  }

  val elements = generateStructure(10)
  println(elements.length)

  new java.io.PrintWriter("Graph.gv") {
    write(tempStruct.myToDot)
    close()
  }

  val outputs = elements.filter(_.isOutput)

  outputs.foreach(tempStruct.registerForPartialPropagation(_))

  tempStruct.close

  tempStruct.validateLayerAssignation





  // val inv0  = new TestInvariantElement(tempStruct)
  // val var2  = new TestVariableElement(tempStruct)
  // val var3  = new TestVariableElement(tempStruct)
  // val var9  = new TestVariableElement(tempStruct)
  // val var10 = new TestVariableElement(tempStruct)

  // val inv1 = new TestInvariantElement(tempStruct)
  // val var4 = new TestVariableElement(tempStruct)
  // val var5 = new TestVariableElement(tempStruct)
  // val var6 = new TestVariableElement(tempStruct)
  // val var7 = new TestVariableElement(tempStruct)
  // val var8 = new TestVariableElement(tempStruct)
  // val var11 = new TestVariableElement(tempStruct)
  // val var12 = new TestVariableElement(tempStruct)

  // inv0.registerStaticAndDynamicDependency(var2)
  // inv0.registerStaticAndDynamicDependency(var3)
  // var9.setDefiningInvariant(inv0)
  // var10.setDefiningInvariant(inv0)

  // inv0.registerStaticAndDynamicDependency(var4)
  // inv1.registerStaticAndDynamicDependency(var3)
  // inv1.registerStaticAndDynamicDependency(var5)
  // inv1.registerStaticAndDynamicDependency(var6)
  // inv1.registerStaticAndDynamicDependency(var7)
  // inv1.registerStaticAndDynamicDependency(var8)
  // var11.setDefiningInvariant(inv1)
  // var12.setDefiningInvariant(inv1)

  // val structure = new TestPropagationStructure

  // for (e <- elements) {

  // }

}
