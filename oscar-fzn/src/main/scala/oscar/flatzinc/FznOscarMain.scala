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
/**
 * @author Jean-Noël Monette
 */
package oscar.flatzinc


trait FznOscarMain extends App{
  val mail = "gustav.bjordal@it.uu.se"
    
  def options(name: String,cbls: Boolean): Options = new Options(name,cbls,args)
  
  def checkAntlr(): Unit ={
      //trick to test whether antlr is on the class path and if not issue a message.
    try{
      val loader = this.getClass().getClassLoader();
      val tmp = loader.loadClass("org.antlr.v4.runtime.CharStream")
    }catch{
      case e: ClassNotFoundException => {
        if(e.getMessage().contains("antlr")){
          System.err.println("Antlr 4.5.1 is not in the classpath.")
          System.err.println("Please put the antlr runtime jar alongside the oscar-fzn.jar")
          System.err.println("You can issue:")
          System.err.println("\twget http://www.antlr.org/download/antlr-runtime-4.5.1.jar")
        }else{
          e.printStackTrace()
        }
      }
    }
  }
  
  def withCheck(code: => Unit): Unit ={
      try{
        code
      } catch {
        case e: UnsatException => {//thrown when some domain is emptied in preprocessing
          println("====UNSATISFIABLE=====")
        }
        case e: ParsingException => {
          System.err.println(e.getMessage())
          System.err.println("\tSyntax error. Aborting.");
          System.err.println("\tIf the flatzinc file is correct, please report to "+mail);
        }
        case e: NoSuchConstraintException => {//might be thrown by the parser or any of the backends
          System.err.println(e.getMessage())
          System.err.println("\tIf this should be an accepted constraint, please report to "+mail)
        }
        case e: Exception => {//catch-all clause...
          //System.err.println(e.getMessage())
          e.printStackTrace()
          System.err.println("\tPlease report the error to "+mail+" with all relevant info and data.")
        }
      }
  }
}