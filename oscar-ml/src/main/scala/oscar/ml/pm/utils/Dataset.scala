/** *****************************************************************************
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
 * *****************************************************************************/


package oscar.ml.pm.utils

/**
 * Handling datasets
 *
 * @author johnaoga@gmail.com
 */

import java.io._

import scala.io.Source.fromFile

/**
 * this object represents a transaction (an itemset or
 * a sequence, the only difference between an itemset
 * and a sequence is that a sequence contains item's repetition)
 */

case class Transaction(data: Array[Int] = Array(),
                       label: Int)


/**
 *
 * This object represents any type of Transaction database (TDB)
 * for Frequent Itemset Mining (FIM) Problem
 *
 */

object Dataset {

  /**
   *
   * @param filename
   * @param format : represents the format of the dataset:
   *               - transaction database,
   *               - transaction database with labels,
   *               - sequence data base,
   *               - single long sequence,...
   * @return
   */
  def apply(filename: String, format: FileFormat = Tdb) = {
    val reader = fromFile(filename)
    val result = reader.getLines().toArray
    reader.close()
    val resultDatas = format.readLines(result)
    new Dataset(filename.slice(filename.lastIndexOf('/') + 1, filename.lastIndexOf('.')), resultDatas)
  }

  def apply(benchmarkName: String, rawDatas: Array[Transaction], nItem: Int) = {
    val data = new Dataset(benchmarkName, rawDatas)
    data.nbItem = nItem
    data
  }

}

/**
 * Generic representation of the data used
 *
 * @param benchmarkName : name of the benchmark
 * @param rawDatas      : Array of all the transactions, transactions are represented by an array containing
 *                      the features (sorted, sparse representation) of the transaction and an int representing the class
 *                      Features numbered from 1 to the number of feature
 */
class Dataset(val benchmarkName: String, val rawDatas: Array[Transaction]) {

  val nbTrans: Int = rawDatas.length
  var nbItem: Int = rawDatas.map(trans => if (trans.data.isEmpty) 0 else trans.data.max).max + 1

  def density(): Double =
    rawDatas.map(_.data.length).sum * 1.0 / (nbTrans * (nbItem - 1))

  def intoVertical(): Array[Set[Int]] =
    Array.tabulate(nbItem)(i => (0 until nbTrans).filter(t => rawDatas(t).data.contains(i)).toSet)

  def getDataset(label: Int): Dataset = {
    Dataset(benchmarkName + ":" + label, rawDatas.filter(_.label == label), nItem = nbItem)
  }

  def splitDatasetByTwo(): (Dataset, Dataset) =
    (getDataset(1), getDataset(0))

  def getData(): Array[Array[Int]] =
    rawDatas.map(_.data)

  def printInfo(file: String, separator: String = "\t"): Unit = {
    val pw = new PrintWriter(new File(file))
    pw.println(stringInfo(separator))
    pw.close()
  }

  override def toString: String = {
    rawDatas.map(_.data.mkString(", ")).mkString("\n")
  }

  def stringInfo(separateur: String = "\t"): String =
    benchmarkName + separateur + nbTrans + separateur + (nbItem - 1) + separateur + density

  def printTo(format: FileFormat): Unit =
    printTo(this.benchmarkName, format)

  def printTo(outputName: String, format: FileFormat): Unit =
    format.writeFile(outputName, this)
}
