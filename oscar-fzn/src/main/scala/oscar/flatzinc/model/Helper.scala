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
package oscar.flatzinc.model

import scala.collection.mutable.{ Map => MMap}

object Helper {
  final val FznMaxInt:Int = Int.MaxValue/100
  final val FznMinInt:Int = Int.MinValue/100
  
//  val cnames = //MMap.empty[Constraint,String]//
//    MMap.empty[Class[_],String]
  def getCName(c: Constraint): String = {
    c match{
      case c:GeneratedConstraint => c.name
      case c:GenericConstraint => c.name
      case reif(c,b) => getCName(c)+"_reif"
      case c => {
        c.getClass().getName().split("\\.").last
//        val cl = c.getClass();
//        if(cnames.contains(cl)){ cnames(cl) }else{
//          val n = cl.getName().split("\\.").last
//          cnames(cl) = n
//          n
//        }
//        if(cnames.contains(c)){ cnames(c) }else{
//          val n = c.getClass().getName().split("\\.").last
//          cnames(c) = n
//          n
//        }
      }
    }
  }
  
  def getCstrsByName(cstrs: Iterable[Constraint]): MMap[String,List[Constraint]] = {
    cstrs.foldLeft(MMap.empty[String,List[Constraint]])((acc,c) => { 
      val name = getCName(c)
      acc(name) = c :: acc.getOrElse(name,List.empty[Constraint]); 
      acc})
  }
  
  
  def getVarDegreeDistribution(vars: Iterable[Variable]):MMap[Int,Int] = {
    val degs = MMap.empty[Int,Int];
    for(v<-vars){
      degs(v.cstrs.size) = degs.getOrElse(v.cstrs.size, 0) + 1
    }
    degs
  }
  def getVarDomainDistribution(vars: Iterable[Variable]):MMap[Int,Int] = {
    val degs = MMap.empty[Int,Int];
    for(v<-vars){
      degs(v.domainSize) = degs.getOrElse(v.domainSize, 0) + 1
    }
    degs
  }
  def getConsDegreeDistribution(cs: Iterable[Constraint]):MMap[Int,Int] = {
    val degs = MMap.empty[Int,Int];
    for(c<-cs){
      degs(c.variables.length) = degs.getOrElse(c.variables.length, 0) + 1
    }
    degs
  }
  def getRealConsDegreeDistribution(cs: Iterable[Constraint]):MMap[Int,Int] = {
    val degs = MMap.empty[Int,Int];
    for(c<-cs){
      degs(c.variables.filter(v => !v.isBound).length) = degs.getOrElse(c.variables.filter(v => !v.isBound).length, 0) + 1
    }
    degs
  }
  def printDegreeDistribution(vars: Iterable[Variable]): Unit ={
    val degs = MMap.empty[Int,Int];
    for(v<-vars){
      degs(v.cstrs.size) = degs.getOrElse(v.cstrs.size, 0) + 1
    }
    degs.keys.toList.sorted.foreach(d => println(d+" :: "+degs(d)))
  }
}