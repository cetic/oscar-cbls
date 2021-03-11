package oscar.anytime.lns.utils

import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.ALNSOperator

import scala.xml.{Elem, Node, NodeBuffer, PrettyPrinter}


object XmlWriter{

  private def resultsToXml(
                            config: String,
                            seed: Int,
                            timeout: Long,
                            instance: String,
                            problem: String,
                            bestKnown: Int,
                            maxObjective: Boolean,
                            solutions: Iterable[CPIntSol],
                            operators: Map[String, Array[ALNSOperator]],
                            scoresHistory: Array[(Long, String, Double)]
                          ): Elem = {

    val xml = new NodeBuffer()

    xml += <config>{config}</config>
    xml += <seed>{seed}</seed>
    xml += <timeout>{timeout}</timeout>
    xml += <instance>{instance}</instance>
    xml += <problem>{problem}</problem>
    xml += <objective_type>{if(maxObjective) "max" else "min"}</objective_type>
    if(bestKnown < Int.MaxValue && bestKnown > Int.MinValue) xml += <best_known_objective>{bestKnown}</best_known_objective>

    xml += (<solutions>{solutions.map(_.asXml)}</solutions>)

    xml += (<operators>{operators.flatMap{
      case (category, operators) => operators.map(_.asXml(category)).toIterable
    }}</operators>)

    xml += (<score_history>{scoresHistory.map(entry =>{
      val (time: Long, name: String, score: Double) = entry
      <score_update>
        <time>{time}</time>
        <operator>{name}</operator>
        <score>{score}</score>
      </score_update>
    })}</score_history>)

    <results>{xml}</results>
  }

  /**
    * writes the specified elements into an xml file.
    * @param directory the path of a directory where the file will be created
    * @param config the name of the config which is the first part of the filename
    * @param instance the name of the instance which is the second part of the filename
    * @param bestKnown the objective value of the best known solution if known.
    * @param maxObjective whether the objective is to maximise (true) or minimise (false).
    * @param solutions an iterable of pairs Time (Long) - solution objective (Int).
    * @return an xml node.
    */
  def writeToXml(
                  directory: String,
                  config: String,
                  seed: Int,
                  timeout: Long,
                  instance: String,
                  problem: String = "unknown",
                  bestKnown: Int = Int.MaxValue,
                  maxObjective: Boolean,
                  solutions: Iterable[CPIntSol],
                  operators: Map[String, Array[ALNSOperator]],
                  scoresHistory: Array[(Long, String, Double)] = Array()

                ): Unit = {
    val xml = resultsToXml(config, seed, timeout, instance, problem, bestKnown, maxObjective, solutions, operators, scoresHistory)
    var output = directory + "/" + problem + "/" + config + "_" + instance + ".xml"
    if(IOUtils.fileExists(output)){
      var i = 0
      do{
        i += 1
        output = directory + "/" + problem + "/" + config + "_" + instance + "_" + i + ".xml"
      }while(IOUtils.fileExists(output))
    }
    IOUtils.saveToFile(output, new PrettyPrinter(80, 4).format(xml))
  }
}