package oscar.cp.minizinc

import java.io.FileReader

object test extends Parser with App{
    test
   
	def test: Unit = {
		//val input = "bool: name = true;"
    	//val input = new FileReader(args(0))
    	//val input = "array [1..7] of set of int: name = [2..5, {8, 7, 5}, {3}];" + "solve satisfy;"
		//println(myParseAll(input))
		myParseAll(new Options(args),Map(""->""))
		//println(myParseAll(new Options(args),Map(""->"")))
		//println(model.dict.toString)
		//println(model.dict.get("name"))
//		model.dict.get("name") match {
//		  case Some(p) => println(p.toString)
//		}
	}	
}
