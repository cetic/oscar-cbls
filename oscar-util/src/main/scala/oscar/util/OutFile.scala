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
package oscar.util

import java.io.{BufferedWriter, FileWriter}

class OutFile(filepath: String, critical: Boolean, verbous: Boolean) {

  val file: BufferedWriter = openFile()

  private def openFile(): BufferedWriter = {
    try new BufferedWriter(new FileWriter(filepath))
    catch {
      case e: Error => {
        errorHandling(e)
        null
      }
    }
  }

  private def errorHandling(e: Error): Unit = {
    if (verbous) println(e.getMessage)
    if (critical) System.exit(-1)
  }

  def write(line: String): Unit = {
    try file.write(line)
    catch {
      case e: Error => errorHandling(e)
    }
  }
  
  def write(line: Int): Unit = {
    write(line)
  }

  def writeln(line: String): Unit = write(line + "\n")
  
  def writeln(line: Int): Unit = writeln(line)
  
  def writeln() = write("\n")

  def close() = {
    try file.close()
    catch {
      case e: Error => errorHandling(e)
    }
  }
}

object OutFile {
  def apply(filepath: String, critical: Boolean = true, verbous: Boolean = true) = new OutFile(filepath, critical, verbous)
}
