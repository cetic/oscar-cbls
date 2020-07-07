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
      Transaction(data.dropRight(1).sorted, data.last)
    } else {
      Transaction(data, -1)
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

object Tdb extends SparseFormat

object TdbWithLabel extends SparseFormat {
  this.withLabel = true
}

/**
 * Sequence dataset
 * T 0 1 0 1 0 1
 * T 1 0 1 0 0 0
 * used by DL8
 */

/*object BinaryPreFormat extends FileFormat {
  val extension: String = ".txt"

  def checkFormatLine(line: String): Unit = {
    val data = line.split(" ")
    val pattern = "[0-1]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  def readLine(line: String): (Array[Int], Int) = {
    val data = line.split(" ").map(_.toInt)
    val buffer = new scala.collection.mutable.ArrayBuffer[Int]()
    var i = 1
    while (i < data.length) {
      if (data(i) == 1)
        buffer += i
      i += 1
    }
    (buffer.toArray, data.head)
  }

  def writeTransaction(transaction: (Array[Int], Int), nbItem: Int): String = {
    val arr = Array.fill(nbItem - 1)(0)
    for (id <- transaction._1)
      arr(id - 1) = 1
    transaction._2 + " " + arr.mkString(" ")
  }
}

/**
 * Format Binaire Pre Target
 * 0;1;0;1;0;1;T
 * 1;0;1;0;0;0;T
 * used by BinOCT
 */
object BinaryPostFormat extends FileFormat {
  val extension: String = ".csv"

  def checkFormatLine(line: String): Unit = {
    val data = line.split(";")
    val pattern = "[0-1]*"
    assert(data.forall(str => str.matches(pattern)))
  }

  override def readLines(lines: Array[String]): Array[(Array[Int], Int)] =
    lines.drop(1).map(readLine(_))

  def readLine(line: String): (Array[Int], Int) = {
    val data = line.split(";").map(_.toInt)
    val buffer = new scala.collection.mutable.ArrayBuffer[Int]()
    var i = 0
    while (i < data.length - 1) {
      if (data(i) == 1)
        buffer += (i + 1)
      i += 1
    }
    (buffer.toArray, data.last)
  }

  override def writeTransactions(printer: PrintWriter, data: Dataset): Unit = {
    printer.println((0 until data.nbItem - 1).map("Feat_" + _).mkString(";") + ";target")
    super.writeTransactions(printer, data)
  }

  def writeTransaction(transaction: (Array[Int], Int), nbItem: Int): String = {
    val arr = Array.fill(nbItem - 1)(0)
    for (id <- transaction._1)
      arr(id - 1) = 1
    arr.mkString(";") + ";" + transaction._2
  }
}*/