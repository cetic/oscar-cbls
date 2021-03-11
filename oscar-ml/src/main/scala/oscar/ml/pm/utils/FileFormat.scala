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

import java.io.{File, PrintWriter}

/**
 * This class represents any kind of datasets
 *
 * @author johnaoga@gmail.com
 */
abstract class FileFormat {
  val extension: String
  val separator: String
  var withLabel: Boolean = false
  var withItemNamesHeader: Boolean = false
  var nSkip = 0

  def readLines(lines: Array[String]): Array[Transaction] = {
    if (!withItemNamesHeader) lines.foreach(checkFormatLine(_))
    lines.drop(nSkip).map(readLine(_))
  }

  def checkFormatLine(line: String): Unit

  def readLine(line: String): Transaction

  def readHeader(lines: Array[String], nItems: Int): Array[String] = {
    Array()
  }

  def writeFile(outputName: String, data: Dataset): Unit = {
    val pw = new PrintWriter(new File(outputName + extension))
    writeTransactions(pw, data)
    pw.close()
  }

  def writeTransactions(printer: PrintWriter, data: Dataset): Unit = {
    var id = 0
    while (id < data.nbTrans) {
      printer.println(writeTransaction(data.rawDatas(id), data.nbItem))
      id += 1
    }
  }

  def writeTransaction(transaction: Transaction, nbItem: Int): String
}

class FunctionNotUsedForThisFileFormatException(s: String) extends Exception(s) {}

/**
 * Format sparse with/without class label
 * 1 2 3 4
 * 1 3 4
 */
class SparseFormat extends FileFormat {
  override val extension: String = ".txt"
  override val separator: String = "\\s+"

  override def checkFormatLine(line: String): Unit = {
    val data = line.split(separator)
    val pattern = "[0-9]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLine(line: String): Transaction = {
    val data = line.split(separator).map(_.toInt)

    if (withLabel) {
      Transaction(data = data.dropRight(1).sorted, label = data.last)
    } else {
      Transaction(data = data)
    }
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    else str
  }

}

object TdbFormat extends SparseFormat

object TdbWithLabelFormat extends SparseFormat {
  this.withLabel = true
}

/**
 * Sequence dataset with time SPADE format
 * The sequence dataset (item, time)
 * (1, 2)(2, 5)(4, 6)(3, 10)(2, 11)
 * (3, 1)(2, 2)
 *
 * becomes
 * sid time  size  item
 * 1    2     1     1
 * 1    5     1     2
 * 1    6     1     4
 * 1   10     1     3
 * 1   11     1     2
 * 2    1     1     3
 * 2    2     1     2
 */
object SpadeFormat extends FileFormat {
  override val extension: String = ".sp"
  override val separator: String = "\\s+"

  val POS_SID = 0
  val POS_EID = 1
  val POS_SIZ = 2
  val POS_ITM = 3

  override def checkFormatLine(line: String): Unit = {
    val data = line.split(separator)
    val pattern = "[0-9]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLine(line: String): Transaction = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLines(lines: Array[String]): Array[Transaction] = {
    lines.foreach(e => checkFormatLine(e))
    val data = lines.map(_.split(separator).map(_.toInt))

    def buildTransaction(sid: Int): Transaction = {
      val trans = data.filter(_ (POS_SID) == sid)
      Transaction(data = trans.map(_ (POS_ITM)), time = trans.map(_ (POS_EID)))
    }

    (1 to data.last(0)).map(t => buildTransaction(t)).toArray

  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    else str
  }

}

/**
 * Sequence dataset with time b-SPADE format
 * The sequence dataset (item, time)
 * (A, 2)(B, 5)(D, 6)(C, 10)(B, 11)
 * (C, 1)(B, 2)
 *
 * becomes
 * nseq seqlenmax item
 * [0, A, B, C, D]
 * sid time  size  item  support
 * 1    2     1     1        1
 * 1    5     1     2        2
 * 1    6     1     4        1
 * 1   10     1     3        2
 * 1   11     1     2        2
 * 2    1     1     3        2
 * 2    2     1     2        2
 */
object BSpadeFormat extends FileFormat {
  override val extension: String = ".b"
  override val separator: String = "\\s+"
  this.withItemNamesHeader = true
  this.nSkip = 2

  val POS_SID = 0
  val POS_EID = 1
  val POS_SIZ = 2
  val POS_ITM = 3
  val POS_SUP = 4

  override def checkFormatLine(line: String): Unit = {
    val data = line.split(separator)
    val pattern = "[0-9]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLine(line: String): Transaction = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLines(lines: Array[String]): Array[Transaction] = {
    lines.drop(nSkip).foreach(e => checkFormatLine(e))
    val data = lines.drop(nSkip).map(_.split(separator).map(_.toInt))

    def buildTransaction(sid: Int): Transaction = {
      val trans = data.filter(_ (POS_SID) == sid)
      Transaction(data = trans.map(_ (POS_ITM)), time = trans.map(_ (POS_EID)))
    }

    (1 to data.last(0)).map(t => buildTransaction(t)).toArray

  }

  override def readHeader(lines: Array[String], nItems: Int): Array[String] = {
    val out = lines(1).drop(1).dropRight(1).split(", ")
    assert(out.length == nItems)
    out
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    else str
  }

}

/**
 * Sequence dataset SPMF format
 * The sequence dataset
 * 1 -1 2 -1 4 -1 3 -1 2 -1 -2
 * 3 -1 2 -1 -2
 */
object SpmfFormat extends FileFormat {
  override val extension: String = ".spmf"
  override val separator: String = " -1 "

  override def checkFormatLine(line: String): Unit = {
    val data = line.trim.stripSuffix(" -1 -2").split(separator)
    val pattern = "[0-9]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLine(line: String): Transaction = {
    val data = line.trim.stripSuffix(" -1 -2").split(separator).map(_.toInt)
    Transaction(data = data)
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    else str
  }

}


/**
 * Sequence dataset SPMF with header format
 * The sequence dataset
 *
 * @ CONVERTED_FROM_TEXT
 * @ ITEM=1=A
 * @ ITEM=2=B
 * @ ITEM=3=C
 * @ ITEM=4=D
 * @ ITEM=-1=|
 * 1 -1 2 -1 4 -1 3 -1 2 -1 -2
 * 3 -1 2 -1 -2
 */
object SpmfWithHeaderFormat extends FileFormat {
  override val extension: String = ".spmf"
  override val separator: String = " -1 "
  this.withItemNamesHeader = true
  this.nSkip = 1

  override def checkFormatLine(line: String): Unit = {
    val data = line.trim.stripSuffix(" -1 -2").split(separator)
    val pattern = "[0-9]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLine(line: String): Transaction = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLines(lines: Array[String]): Array[Transaction] = {
    nSkip = lines.count(_.trim.startsWith("@"))
    val realLines = lines.filterNot(_.trim.startsWith("@"))

    realLines.foreach(e => checkFormatLine(e))
    val data = realLines.map(line => line.trim.stripSuffix(" -1 -2").split(separator).map(e => ("""\d+""".r findAllIn e).toList))

    data.indices.map(t => Transaction(data = data(t).map(_.last.toInt), time = data(t).map(_.head.toInt))).toArray
  }

  override def readHeader(lines: Array[String], nItems: Int): Array[String] = {
    val tab = lines.filter(_.trim.startsWith("@ITEM")).map(e => e.trim.split("=")) //@ITEM=4=D

    val out: Array[String] = Array.fill(nItems + 1)("0")

    var i = 0
    while (i < tab.length) {
      out(tab(i)(1).toInt) = tab(i)(2)
      i += 1
    }

    out
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    else str
  }

}

/**
 * Sequence dataset SPMF format
 * The sequence dataset < time > item
 * <2> 1 -1 <5> 2 -1 <6> 4 -1 <10> 3 -1 <11> 2 -1 -2
 * <1> 3 -1 <2> 2 -1 -2
 */
object SpmfWithTimeFormat extends FileFormat {
  override val extension: String = ".spmf"
  override val separator: String = " -1 "

  override def checkFormatLine(line: String): Unit = {
    val data = line.trim.stripSuffix(" -1 -2").split(separator)
    val pattern = "[0-9]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLine(line: String): Transaction = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLines(lines: Array[String]): Array[Transaction] = {
    lines.drop(nSkip).foreach(e => checkFormatLine(e))
    val data = lines.drop(nSkip).map(line => line.trim.stripSuffix(" -1 -2").split(separator).map(e => ("""\d+""".r findAllIn e).toList))

    data.indices.map(t => Transaction(data = data(t).map(_ (1).toInt), time = data(t).map(_ (0).toInt))).toArray
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    else str
  }

}

/**
 * Long Sequence dataset format
 * The long sequence is
 * 1 2 2 2 1 2 1 2 1
 *
 * but can be represented (with or without spaces)
 * 1 2 2 2
 * 1 2
 * 1 2 1
 */
class LongSequenceFormat extends FileFormat {
  override val extension: String = ".txt"
  override val separator: String = "\\s+"

  val POS_ITM = 0
  val POS_EID = 1

  override def checkFormatLine(line: String): Unit = {
    val data = line.split(separator)
    val pattern = "[0-9]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLine(line: String): Transaction = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLines(lines: Array[String]): Array[Transaction] = {
    lines.foreach(e => checkFormatLine(e))
    Array(Transaction(data = lines.flatMap(_.split(separator).map(_.toInt))))
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    str
  }

}

/**
 * Long Sequence dataset with time format
 * The sequence dataset
 * item time
 * 1    1
 * 2    3
 * 1    5
 * 3    6
 * 2    7
 * 1    8
 * 2   14
 * This is the format used by trade market and ubiqlog datasets
 */
object LongSequenceTimeFormat extends FileFormat {
  override val extension: String = ".txt"
  override val separator: String = "\\s+"

  val POS_ITM = 0
  val POS_EID = 1

  override def checkFormatLine(line: String): Unit = {
    val data = line.split(separator)
    val pattern = "[0-9]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLine(line: String): Transaction = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLines(lines: Array[String]): Array[Transaction] = {
    lines.foreach(checkFormatLine(_))
    val data = lines.map(_.split(separator).map(_.toInt))

    Array(Transaction(data = data.map(_ (POS_ITM)), time = data.map(_ (POS_EID))))
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    str
  }

}

/**
 * Sequence dataset Protein format
 * The sequence dataset
 *
 * ABCDEA
 * ABC
 */
object ProteinLongSequence extends FileFormat {
  override val extension: String = ".fasta"
  override val separator: String = ""
  this.withItemNamesHeader = true
  this.nSkip = 1

  override def checkFormatLine(line: String): Unit = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLine(line: String): Transaction = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLines(lines: Array[String]): Array[Transaction] = {
    Array(Transaction(data = lines.flatMap(line => line.trim.toCharArray.map(e => e - 'A' + 1))))
  }

  override def readHeader(lines: Array[String], nItems: Int): Array[String] = {
    "eps." +: ('A' to 'Z').map(_.toString).toArray
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    else str
  }

}


/**
 * Sequence dataset Protein format
 * The sequence dataset
 *
 * A 1
 * B 2
 * C 5
 */
object LongSequenceWithNameAndTime extends FileFormat {
  override val extension: String = ".txt"
  override val separator: String = " "
  this.withItemNamesHeader = true
  this.nSkip = 1
  private[this] val revMap = collection.mutable.Map.empty[String, Int]

  override def checkFormatLine(line: String): Unit = {
    val data = line.trim.split(separator)
    val pattern = "[0-9]*"
    assert(data(1).matches(pattern))
  }

  override def readLine(line: String): Transaction = {
    throw new FunctionNotUsedForThisFileFormatException("This function is not used for this file format")
  }

  override def readLines(lines: Array[String]): Array[Transaction] = {
    var count = 1
    val seqs: Array[Int] = Array.ofDim(lines.length)
    val timestamps: Array[Int] = Array.ofDim(lines.length)

    var i = 0
    while (i < lines.length) {
      val line = lines(i).split(separator)
      val symbol = line(0)
      val timestamp = line(1).toInt

      if (revMap.contains(symbol)) {
        seqs(i) = revMap(symbol)
        timestamps(i) = timestamp
      } else {
        seqs(i) = count
        timestamps(i) = timestamp
        revMap(symbol) = count
        count += 1
      }
      i += 1
    }

    Array(Transaction(data = seqs, time = timestamps))
  }

  override def readHeader(lines: Array[String], nItems: Int): Array[String] = {
    "eps." +: revMap.map(_.swap).values.toArray
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    else str
  }

}