package oscar.cbls.test.core.propagation



import oscar.cbls.core.propagation.{
  TestPropagationStructure,
  TestPropagationElement,
  TestInvariantElement,
  TestVariableElement}
import scala.annotation.tailrec

class PropagationStructureGenerator {

  val rand = new scala.util.Random(1000)

  private def generateInt(min : Int,max : Int) = {
    min + rand.nextInt(max - min)
  }


  private def generateVariables(min: Int,
    max: Int,
    struct : TestPropagationStructure,
    predecessor : Option[TestInvariantElement] = None
  ): List[TestVariableElement] = {
    @tailrec
    def generateList(
      nb: Int = generateInt(min,max),
      res: List[TestVariableElement] = List()
    ): List[TestVariableElement] = {
      if (nb == 0) {
        res
      } else {
        val newVar = new TestVariableElement(struct)
        newVar.theoreticalLayer = predecessor.map(_.theoreticalLayer +1).getOrElse(0)
        predecessor match {
          case Some(i) => newVar.setDefiningInvariant(i)
          case _ => 
        }
        generateList(nb - 1, newVar :: res)
      }
    }
    generateList()
  }

  private def generateInvariantLayer(
    variablePool: List[TestVariableElement],
    struct : TestPropagationStructure,
    minInvariantPerLayer : Int,
    maxInvariantPerLayer : Int,
    nbMinInput : Int,
    nbMaxInput : Int,
    nbMinOutput : Int,
    nbMaxOutput : Int
  ): (List[TestInvariantElement], List[TestVariableElement]) = {

    @tailrec
    def generateList(
      nbInvToGenerate: Int = generateInt(minInvariantPerLayer,maxInvariantPerLayer),
      generatedInvariants: List[TestInvariantElement] = List(),
      generatedOutVar: List[TestVariableElement] = List()
    ): (List[TestInvariantElement], List[TestVariableElement]) = {
      if (nbInvToGenerate == 0) {
        (generatedInvariants, generatedOutVar)
      } else {
        val newInvariant = new TestInvariantElement(struct)
        val input = rand.shuffle(variablePool).take(generateInt(nbMinInput,nbMaxInput))
        newInvariant.theoreticalLayer = input.map(_.theoreticalLayer).max + 1
        input.foreach(v => newInvariant.registerStaticAndDynamicDependency(v))
        val output = generateVariables(nbMinOutput,nbMaxOutput,struct,Some(newInvariant))
        generateList(nbInvToGenerate - 1, newInvariant :: generatedInvariants, output ::: generatedOutVar)
      }
    }
    generateList()
  }

  @tailrec
  private def insertElem(
    e: TestPropagationElement,
    l: List[TestPropagationElement],
    res: List[TestPropagationElement] = List()
  ): List[TestPropagationElement] = {
    l match {
      case Nil    => e :: res
      case h :: t => if (h == e) insertElem(e, t, res) else insertElem(e, t, h :: res)
    }
  }


  private def generateElements(inputVar : List[TestVariableElement],
    nbLayers : Int,
    struct : TestPropagationStructure,
    minElementPerLayer : Int,
    maxElementPerLayer : Int,
    minInput : Int,
    maxInput : Int,
    minOutput : Int,
    maxOutput : Int) : List[TestPropagationElement] = {
    def generateLayers(
      pool: List[TestVariableElement] = inputVar,
      currentMaxLayer: Int = 0,
      elements: List[TestPropagationElement] = List(),
    ): List[TestPropagationElement] = {
      if (currentMaxLayer >= nbLayers) {
        elements
      } else {
        val invAndOut = generateInvariantLayer(pool,
          struct,
          minElementPerLayer,
          maxElementPerLayer,
          minInput,
          maxInput,
          minOutput,
          maxOutput)
        val newPool   = invAndOut._2 ::: pool
        val maxLayer  = newPool.map(_.theoreticalLayer).max
        val newElems =
          (invAndOut._1 ::: newPool).foldLeft(elements)((elems, e) => insertElem(e, elems))
        generateLayers(newPool, maxLayer, newElems)
      }
    }
    generateLayers()
  }


  def generateStructure(minElemPerLayer : Int,
    maxElemPerLayer : Int,
    minInputVar : Int,
    maxInputVar : Int,
    minOutputVar : Int,
    maxOutputVar : Int,
    nbLayer : Int) : (TestPropagationStructure,List[TestPropagationElement]) = {

    val struct = new TestPropagationStructure

    val input = generateVariables(minElemPerLayer,maxElemPerLayer,struct)

    val elements = generateElements(input,nbLayer,struct,minElemPerLayer,maxElemPerLayer,minInputVar,maxInputVar,minOutputVar,maxOutputVar)

    (struct,elements)



  }




}
