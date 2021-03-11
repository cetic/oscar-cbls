/*******************************************************************************
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
 ******************************************************************************/

package oscar.ml.classificationtree.DataManipulation

import java.io._
import scala.io.Source.fromFile
import scala.util.Random
import oscar.algo.reversible.BitSetOp
/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */

object Data {

  def apply(file: String, format: FileFormat=SparseFormat) = {
    val reader = fromFile(file)
    val result = reader.getLines().toArray
    reader.close()
    val resultDatas = format.readLines(result)
    new Data(file.slice(file.lastIndexOf('/') + 1, file.lastIndexOf('.')), resultDatas)
  }

}

/**
  * Generic representation of the data used
  * @param benchmarkName : name of the benchmark
  * @param rawDatas : Array of all the transactions, transactions are represented by an array containing
  *                 the features (sorted, sparse representation) of the transaction and an int representing the class
  *                 Features numbered from 1 to the number of feature
  */
class Data(val benchmarkName: String, val rawDatas: Array[(Array[Int], Int)]) {

//  println(rawDatas.map(x => x._1.mkString(",") + " " + x._2).mkString("\n"))
  val nbTrans: Int = rawDatas.length
  val nbItem: Int = rawDatas.maxBy(items => if (items._1.isEmpty) 0 else items._1.last)._1.last + 1
  // TODO put the following as lazy?
  val nbTransM: Int = rawDatas.count(_._2 == 0)
  val nbTransP: Int = rawDatas.count(_._2 == 1)
  val errorUB: Int = Math.min(nbTransP, nbTransM)
  val (dataM, dataP) = {
    val dM = Array.fill(nbItem)(Array.fill(BitSetOp.bitLength(nbTransM))(0L))
    var idM = 0
    val dP = Array.fill(nbItem)(Array.fill(BitSetOp.bitLength(nbTransP))(0L))
    var idP = 0
    var i = 0
    while (i < nbTrans) {
      if (rawDatas(i)._2 == 0) {
        for (item <- rawDatas(i)._1)
          BitSetOp.setBit(dM(item), idM)
        idM += 1
      } else {
        for (item <- rawDatas(i)._1)
          BitSetOp.setBit(dP(item), idP)
        idP += 1
      }
      i += 1
    }
    (dM, dP)
  }
  val (dataMI, dataPI) = {
    val dM = Array.tabulate(nbItem){
      x =>
        val cl = dataM(x).clone()
        for (i <- cl.indices)
          cl(i) = ~cl(i)
        cl
    }
    val dP = Array.tabulate(nbItem){
      x =>
        val cl = dataP(x).clone()
        for (i <- cl.indices)
          cl(i) = ~cl(i)
        cl
    }
    (dM, dP)
  }

  val density =
    rawDatas.map(_._1.length).sum * 1.0 / (nbTrans * (nbItem-1))

  def printInfo(file:String,separateur:String="\t"):Unit = {
    val pw = new PrintWriter(new File(file))
    pw.println(stringInfo(separateur))
    pw.close()
  }
  def stringInfo(separateur:String="\t"):String = {
    benchmarkName+separateur+nbTrans+"("+nbTransP+"-"+nbTransM+")"+separateur+(nbItem-1) + separateur + density
  }

  def printTo(format: FileFormat):Unit =
    printTo(this.benchmarkName,format)

  def printTo(outputName: String, format: FileFormat):Unit =
    format.writeFile(outputName,this)


  def sample(nb: Int, id: String, format: FileFormat) = {
    val randomArray = Array.tabulate(nbTrans)(i => (i, Random.nextInt())).sortBy(_._2).map(x => rawDatas(x._1))
    val (train,test) = randomArray.splitAt(nb)
    val trainData = new Data(benchmarkName + "_train_" + id,train)
    val testData = new Data(benchmarkName + "_test_" + id,test)
    trainData.printTo(format)
    testData.printTo(format)
  }
}