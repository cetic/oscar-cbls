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

  def readLines(lines: Array[String]): Array[Transaction] = {
    lines.foreach(checkFormatLine(_))
    lines.map(readLine(_))
  }

  def checkFormatLine(line: String): Unit

  def readLine(line: String): Transaction

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

class FunctionNotUsedForThisFileFormatException(s:String) extends Exception(s){}

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
    str
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
 *sid time  size  item
 *  1    2     1     1
 *  1    5     1     2
 *  1    6     1     4
 *  1   10     1     3
 *  1   11     1     2
 *  2    1     1     3
 *  2    2     1     2
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

    def buildTransaction(sid:Int): Transaction = {
      val trans =  data.filter(_(POS_SID) == sid)
      Transaction(data = trans.map(_(POS_ITM)), time = trans.map(_(POS_EID)))
    }

    (1 to data.last(0)).map(t => buildTransaction(t)).toArray

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
    Array(Transaction(data = lines.map(_.split(separator).map(_.toInt)).flatten))
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
 *  1    1
 *  2    3
 *  1    5
 *  3    6
 *  2    7
 *  1    8
 *  2   14
 *  This is the format used by trade market and ubiqlog datasets
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

    Array(Transaction(data = data.map(_(POS_ITM)), time = data.map(_(POS_EID))))
  }

  override def writeTransaction(transaction: Transaction, nbItem: Int): String = {
    var str = ""
    for (item <- transaction.data)
      str += (item - 1) + " "
    if (withLabel) str + transaction.label
    str
  }

}