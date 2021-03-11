package oscar.ml.classificationtree

import DataManipulation.{Data, FileFormat}

/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
class Validation(treeString: String) {

  // tree syntax : feature(yesTree, noTree) or 0:x (leaf of class 0 with x being the error of leaf) or 1:x (leaf of class 1 with x being the error of leaf) or 2:x if indertermine

  // <Tree>    ::= <Leaf> | <Feature>(<Tree>,<Tree>)
  // <Feature> ::= 0..(m-1)
  // <Leaf>    ::= <Class>:<Error>
  // <Class>   ::= 0 | 1 | 2                            // 2 means indefined class, should barely happen
  // <Error>   ::= 0..(n/2)

  abstract class ValidationTree {
    val isLeaf:Boolean
    def classify(transaction:Set[Int]):Int
    def validate(transaction:Set[Int],classification:Int):Boolean = {
      val c = classify(transaction)
      (c == 2) || (c == classification)
    }
  }

  case class NodeValidationTree(feature: Int, yesBranch: ValidationTree, noBranch: ValidationTree) extends ValidationTree {
    val isLeaf = false
    def classify(transaction:Set[Int]):Int ={
      if (transaction.contains(feature+1))
        yesBranch.classify(transaction)
      else
        noBranch.classify(transaction)
    }
  }

  case class LeafValidationTree(leafClass: Int) extends ValidationTree{
    val isLeaf = true
    def classify(transaction:Set[Int]):Int =
      leafClass
  }

  val tree = {
    var index = 0

    def locateInt(from:Int) : (Int,Int) = {
      var to = from
      while (Character.isDigit(treeString.charAt(to)))
        to += 1
      (to,treeString.slice(from, to).toInt)
    }
    def createTree(): ValidationTree = {

      val (to, value) = locateInt(index)
      index = to + 1
      treeString.charAt(to) match {
        case ':' =>
          val (to, _) = locateInt(index)
          index = to
          LeafValidationTree(value)
        case '(' =>
          val yesBranch = createTree()
          if (treeString.charAt(index) != ',')
            throw new Error("tree not of the right form")
          index += 1
          val noBranch = createTree()
          if (treeString.charAt(index) != ')')
            throw new Error("tree not of the right form")
          index += 1
          NodeValidationTree(value, yesBranch, noBranch)
        case _ => throw new Error("tree not of the right form")
      }
    }
    createTree()
  }


  def validate(file: String,format:FileFormat) = {
    val data = Data(file,format)
    var goodClass = 0
    for ((transaction, classification) <- data.rawDatas){
      if (tree.validate(transaction.toSet,classification))
        goodClass += 1
    }
    val wrongClass = data.nbTrans - goodClass
    println("TestError: " + wrongClass)
    println("TestSize: " + data.nbTrans)
    println("TestAccuracy: " + (goodClass * 1.0 / data.nbTrans))
  }


}
