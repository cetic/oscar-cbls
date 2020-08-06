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
                       label: Int = 1,
                       time: Array[Int] = Array())
//We don't take into account sequence of itemsets in which case data should be Array[Set[Int]] instead of Array[Int]

class InvalidOperationException(s:String) extends Exception(s){}

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
  def apply(filename: String, format: FileFormat = TdbFormat) = {
    val reader = fromFile(filename)
    val result = reader.getLines().toArray
    reader.close()
    val resultDatas = format.readLines(result)

    val data = new Dataset(filename.slice(filename.lastIndexOf('/') + 1, filename.lastIndexOf('.')), resultDatas)
    data.usedFormat = format
    if (format.withItemNamesHeader) data.itemsStringsMap = format.readHeader(result, data.nbItem)
    data
  }

  def apply(benchmarkName: String, rawDatas: Array[Transaction], nItem: Int) = {
    val data = new Dataset(benchmarkName, rawDatas)
    data.nbItem = nItem
    data
  }

  def apply(rawDatas: Array[Array[Int]]): Dataset = {
    new Dataset("Dataset", rawDatas.map(e => Transaction(data = e)))
  }

  def apply(data:Array[Array[Int]], time:Array[Array[Int]]): Dataset = {
    new Dataset("Dataset", data.zip(time).map(e => Transaction(data = e._1, time = e._2)))
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
  var usedFormat:FileFormat = TdbFormat
  var itemsStringsMap: Array[String] = (0 to nbItem).map(_.toString).toArray

  def density(): Double =
    rawDatas.map(_.data.length).sum * 1.0 / (nbTrans * (nbItem - 1))

  def intoVertical(): Array[Set[Int]] =
    Array.tabulate(nbItem)(i => (0 until nbTrans).filter(t => rawDatas(t).data.contains(i)).toSet)

  def getDataset(label: Int): Dataset = {
    Dataset(benchmarkName + ":" + label, rawDatas.filter(_.label == label), nItem = nbItem)
  }

  def splitDatasetByTwo(): (Dataset, Dataset) =
    (getDataset(1), getDataset(0))

  def getData: Array[Array[Int]] =
    rawDatas.map(_.data)

  def getDataWithName: Array[Array[String]] =
    rawDatas.map(_.data.map(i => itemsStringsMap(i)))

  def getTime: Array[Array[Int]] = {
    // create a time dataset, if no time dataset is provided the array indices are used
    rawDatas.map(elt => if(elt.time.isEmpty && !elt.data.isEmpty) (1 to elt.data.length).toArray else elt.time)
  }

  def getLabels: Array[Int] =
    rawDatas.map(_.label)

  def printInfo(file: String, separator: String = "\t"): Unit = {
    val pw = new PrintWriter(new File(file))
    pw.println(stringInfo(separator))
    pw.close()
  }

  def patternToString(pattern: Array[Int], sep:String = " "): String = {
    pattern.map(i => itemsStringsMap(i)).mkString(sep)
  }

  override def toString: String = {
    rawDatas.map(trans => patternToString(trans.data) ).mkString("\n")
  }

  def stringInfo(separateur: String = "\t"): String =
    benchmarkName + separateur + nbTrans + separateur + (nbItem - 1) + separateur + density

  def printTo(format: FileFormat): Unit =
    printTo(this.benchmarkName, format)

  def printTo(outputName: String, format: FileFormat): Unit =
    format.writeFile(outputName, this)
}

object DatasetUtils {

  /// Helpers
  def prepareForSPM(filename: String, minsup:Double, format: FileFormat = TdbFormat): (Dataset, Int, Int, Int, Int, Array[Int]) = {
    val primedb = Dataset(filename, format)
    var frequency = minsup.intValue()
    if (minsup > 0 && minsup < 1) frequency = (minsup * primedb.nbTrans).ceil.toInt //floor is another way around for the support

    val db = DatasetUtils.cleanDataset(primedb, frequency)

    (db, frequency, db.nbTrans, db.nbItem, DatasetUtils.getLenSeqMax(db), DatasetUtils.getFrequentItems(db, frequency))
  }

  def prepareForSPMTime(filename: String, minsup:Double, format: FileFormat = SpadeFormat): (Dataset, Int, Int, Int, Int, Array[Int], Int) = {
    val db = Dataset(filename, format)
    var frequency = minsup.intValue()
    if (minsup > 0 && minsup < 1) frequency = (minsup * db.nbTrans).ceil.toInt //floor is another way around for the support

    (db, frequency, db.nbTrans, db.nbItem, DatasetUtils.getLenSeqMax(db), DatasetUtils.getFrequentItems(db, frequency), db.getTime.map(_.max).max)
  }

  def prepareForFEM(filename: String, minsup:Double, format: FileFormat = ProteinLongSequence): (Dataset, Int, Int, Int, Int, Array[Int]) = {
    val db = Dataset(filename, format)
    var frequency = minsup.intValue()
    if (minsup > 0 && minsup < 1) frequency = (minsup * db.rawDatas(0).data.length).ceil.toInt //floor is another way around for the support

    (db, frequency, db.rawDatas(0).data.length, db.nbItem, DatasetUtils.getLenSeqMax(db), DatasetUtils.getLSFrequentItems(db, frequency))
  }

  def prepareForFEMTime(filename: String, minsup:Double, format: FileFormat = LongSequenceTimeFormat): (Dataset, Int, Int, Int, Int, Array[Int]) = {
    val db = Dataset(filename, format)
    var frequency = minsup.intValue()
    if (minsup > 0 && minsup < 1) frequency = (minsup * db.rawDatas(0).data.length).ceil.toInt //floor is another way around for the support

    (db, frequency, db.rawDatas(0).data.length, db.nbItem, DatasetUtils.getLenSeqMax(db), DatasetUtils.getLSFrequentItems(db, frequency))
  }

  def precomputedDatastructures(data: Dataset): (Array[Int], Array[Array[Int]], Array[Array[Int]], Array[Array[Int]]) = {
    val lastPosMap = getItemLastPosBySequence(data)
    (getSDBSupport(data),
      getItemFirstPosBySequence(data),
      lastPosMap,
      getSDBLastPos(data, lastPosMap))
  }

  /// Generic Dataset
  def cleanDataset(data:Dataset, minsup: Int, lmin:Int = 1) : Dataset = {
    val sups = getSDBSupport(data)

    val out = data.rawDatas.map( t =>
      if(t.time.isEmpty)
        t.copy(data = t.data.filter(i => sups(i-1) >= minsup))
      else
        t.copy(data = t.data.filter(i => sups(i-1) >= minsup), time = t.time.zipWithIndex.filter(i => sups(t.data(i._2)-1) >= minsup ).map(_._1))
    ).filter(_.data.length >= lmin)

    val newData: Dataset = new Dataset(data.benchmarkName, out)
    newData.itemsStringsMap = data.itemsStringsMap
    newData.usedFormat = data.usedFormat

    newData
  }

  /// SPM dataset
  def getNItemMaxPerSeq(data: Dataset): Int = {
    data.rawDatas.map(_.data.distinct.length).max
  }

  def getLenSeqMax(data: Dataset): Int = {
    data.rawDatas.map(_.data.length).max
  }

  def getSDBSupport(data:Dataset) :  Array[Int] = {
    (0 until (data.nbItem-1) ).map(i => data.rawDatas.count(_.data.contains(i+1))).toArray
  }

  def getFrequentItems(data: Dataset, minsup: Int) : Array[Int] = {
     getSDBSupport(data).zipWithIndex.filter(_._1 >= minsup).map(_._2 + 1)
  }

  def getSDBLastPos(data:Dataset, lastPosMap: Array[Array[Int]]): Array[Array[Int]] ={
    //data.rawDatas.map( t => t.data.map(i => t.data.lastIndexOf(i) + 1) )

    /*val lastPosList = Array.ofDim[Array[Int]](data.nbTrans)
    var sid = 0

    while (sid < data.nbTrans) {
      var j = 0
      val len = data.rawDatas(sid).data.length
      val tempLastNext = Array.ofDim[Int](len)
      while (j < len) {
        val tab = lastPosMap(sid).filter(p => p != 0 && p > j + 1)
        if (!tab.isEmpty) {
          tempLastNext(j) = tab.min
        }
        j += 1
      }
      lastPosList(sid) = tempLastNext
      sid += 1
    }

    lastPosList*/
    data.rawDatas.indices.map(sid => data.rawDatas(sid).data.reverse.distinct.map(i => lastPosMap(sid)(i) ) ).toArray

  }

  def getItemLastPosBySequence(data:Dataset) : Array[Array[Int]] = {
    //(0 until data.nbItem).toArray.map(i => data.rawDatas.map(t => t.data.lastIndexOf(i) + 1))
    data.rawDatas.map( t => (0 to data.nbItem).toArray.map(i => t.data.lastIndexOf(i) + 1) )
  }

  def getItemFirstPosBySequence(data: Dataset): Array[Array[Int]] = {
    //(0 until data.nbItem).toArray.map(i => data.rawDatas.map(t => t.data.indexOf(i) + 1))
    data.rawDatas.map( t => (0 to data.nbItem).toArray.map(i => t.data.indexOf(i) + 1) )
  }

  def getSDBNextPosGap(data:Dataset, minimumGap:Int): Array[Array[Int]] ={
    if (data.rawDatas(0).time.isEmpty) throw new InvalidOperationException("Time dataset is not provided!")

    def posWhere(tab:Array[Int], comp:(Int, Int)): Int ={
      val res = tab.indexWhere(_ >= (comp._1 + minimumGap), from = comp._2)
      if (res == -1) tab.length + 1 else res
    }
    data.rawDatas.map( t => t.time.zipWithIndex.map(e => posWhere(t.time, e) ) )
  }


  /// FEM

  def getLSNextPosGap(data:Dataset, maximumSpan:Int): Array[Array[Int]] ={
    if (data.rawDatas(0).time.isEmpty) throw new InvalidOperationException("Time dataset is not provided!")
    val lsData = data.rawDatas(0)
    val seqLen = lsData.data.length

    /**/def getRightPos(sid:Int, item:Int): Int = {
      val res = lsData.data.lastIndexOf(item, lsData.time.count(_ <= lsData.time(sid) + maximumSpan)-1)

      if( res < sid ) -1 else res
    }
    lsData.data.indices.map( t => (0 until data.nbItem).toArray.map(i => getRightPos(t, i)) ).toArray/**/

    /*val lastPos = Array.fill(seqLen, data.nbItem)(-1)
    var i = 0
    while (i < seqLen ) {
      var j = i
      while(j < seqLen && lsData.time(j) <= (lsData.time(i) + maximumSpan) ) {
        lastPos(i)(lsData.data(j)) = j
        j += 1
      }
      i += 1
    }
    lastPos*/
  }

  def getLSSupport(data:Dataset) :  Array[Int] = {
    (0 until (data.nbItem-1) ).map(i => data.rawDatas(0).data.count(_ == (i+1))).toArray
  }

  def getLSFrequentItems(data: Dataset, minsup: Int) : Array[Int] = {
    getLSSupport(data).zipWithIndex.filter(_._1 >= minsup).map(_._2 + 1)
  }

}


