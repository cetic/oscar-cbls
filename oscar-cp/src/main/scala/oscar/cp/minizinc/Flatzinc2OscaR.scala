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

package oscar.cp.minizinc

import java.io.FileReader
import scala.io.Source
import java.util.regex.Pattern
import oscar.cp.core.NoSolutionException

object FlatZinc2OscaR extends Parser with App {

  parse(args)

  def parse(args: Array[String]): Unit = {
    // "constraint bool2int(BOOL____00771, INT____00772) :: defines_var(INT____00772);"
    val opts = new Options(args)
    //val  pattern = Pattern.compile("\\(.*\\)");
    var lines = Source.fromFile(opts.fileName).getLines.filter(_.contains("bool2int")).map(s => s.substring(s.indexOf('(') + 1, s.indexOf(')')));
    val mapBool2Int = lines.map(_.split(",").reverse.map(_.trim)).map(s => s(0) -> s(1)).toMap

    try {
      myParseAll(opts, mapBool2Int)

    } catch {
      case e: NoSolutionException => println("=====UNSATISFIABLE=====")
      case e: Throwable => {
        println(e)
        println(e.printStackTrace())
        throw new Exception("adding the constraint failed")
      }
    }

    reset
  }
}
