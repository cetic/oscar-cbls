package oscar.anytime.lns.utils

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.xml.{Node, XML}

/**
  * Utility class to generate an html report from a benchmark directory.
  */
//TODO: add search information
object HtmlReporter extends App{
  if(args.length == 1) generateHtml(args(0))
  else println("Incorrect number of arguments!")

  /**
    * Generates an html report from the given xml object.
    */
  def generateHtml(directory: String): Unit = {
    val (timeout, instances, configs, instanceTypes, bestknownSolutions, data) = scanInstances(directory)
    val nBks = bestknownSolutions.values.count(_.isDefined)
    val (bestSols, anyTimeScores, anyTimeGaps, anyTimeQuality, opStats, instanceStats) = processStats(instances, configs, data, nBks)

    val htmlWriter = new HtmlWriter("oscar-perf/src/main/scala/oscar/anytime/lns/utils/chart_report_template.html", directory + "/" + IOUtils.getFileName(directory) + "_htmlReport.html")
    htmlWriter.addHeading("Benchmark configuration")

    htmlWriter.addParagraph("Time allocated: " + timeout/1000000000.0 + " seconds")

    htmlWriter.addHeading("Search configurations", 2)
    configs.foreach(config => {
      htmlWriter.addParagraph(config)
    })

    htmlWriter.addHeading("General statistics")

    htmlWriter.addHeading("Best solution found per method", 2)
    htmlWriter.addElement(
      "table",
      "Best solution found",
      HtmlWriter.tableToHtmlString(renderBestSols(bestSols, configs, instances, instanceTypes.toMap, bestknownSolutions.toMap, showStd = true, showGap = true))
    )

    htmlWriter.addHeading("Score", 3)
    htmlWriter.addElement(
      "line",
      "Anytime score",
      HtmlWriter.tableToHtmlString(renderScoresByTime(anyTimeScores.toSeq, configs, timeout, stepped = true))
    )

    val anyTimeGapsArray = renderGapsByTime(anyTimeGaps.toSeq, configs, timeout)
    if(anyTimeGapsArray.length > 1)
      htmlWriter.addHeading("Distance to BKS", 3)
      htmlWriter.addElement(
        "line",
        "Anytime relative distance to BKS",
        HtmlWriter.tableToHtmlString(anyTimeGapsArray)
      )

    htmlWriter.addHeading("Quality", 3)
    htmlWriter.addElement(
      "line",
      "Anytime quality",
      HtmlWriter.tableToHtmlString(renderQualityByTime(anyTimeQuality.toSeq, configs, timeout))
    )

    htmlWriter.addHeading("Operators statistics")

    opStats.foreach{case (config, instStats) =>
      if(config != "Baseline-best" && config != "Baseline-worst" && config != "Baseline") {
        htmlWriter.addHeading(config, 3)
        htmlWriter.addElement(
          "table",
          config + " operators Stats",
          HtmlWriter.tableToHtmlString(renderOpStats(config, instStats))
        )
      }
    }

    htmlWriter.addHeading("Instances statistics")

    instanceStats.sortBy(_._1).foreach{case(name, sols, scores, gaps, opScores) =>
      htmlWriter.addHeading(name, 2)

      htmlWriter.addElement(
        "line",
        "Search evolution",
        HtmlWriter.tableToHtmlString(renderSolsByTime(sols, configs, timeout, stepped = true))
      )

      opScores.foreach{case (config, operators, opWeights) =>
        if(config != "Baseline-best" && config != "Baseline-worst" && config != "Baseline") {
          htmlWriter.addHeading("Anytime " + config + "'s operators scores", 3)
          htmlWriter.addElement(
            "line",
            "Operator weights evolution for " + config,
            HtmlWriter.tableToHtmlString(renderOpScoresByTime(opWeights, operators, stepped = true))
          )
        }
      }

//      htmlWriter.addElement(
//        "line",
//        "Score evolution",
//        HtmlWriter.tableToHtmlString(renderScoresByTime(scores, configs, timeout, stepped = true))
//      )

//      if(gaps.isDefined)
//        htmlWriter.addElement(
//          "line",
//          "Gap evolution",
//          HtmlWriter.tableToHtmlString(renderGapsByTime(gaps.get, configs, timeout, stepped = true))
//        )
    }

    htmlWriter.writeToHtml()
  }


  //Scans each config_instance file
  def scanInstances(directory: String): (
    Long,
    Seq[String],
    Seq[String],
    mutable.Map[String, String],
    mutable.Map[String, Option[Int]],
    mutable.Map[String, (Boolean, Option[Int], mutable.Map[String, ArrayBuffer[Seq[(Long, Int)]]], mutable.Map[String, Seq[String]], mutable.Map[String, mutable.Map[String, ArrayBuffer[(Int, Long, Int)]]], mutable.Map[String, Seq[(Long, String, Double)]])]
  ) = {

    val data = mutable.Map[String, (Boolean, Option[Int], mutable.Map[String, ArrayBuffer[Seq[(Long, Int)]]], mutable.Map[String, Seq[String]], mutable.Map[String, mutable.Map[String, ArrayBuffer[(Int, Long, Int)]]], mutable.Map[String, Seq[(Long, String, Double)]])]()
    val instances = mutable.HashSet[String]()
    val instanceTypes = mutable.Map[String, String]()
    val bks = mutable.Map[String, Option[Int]]()
    val configs = mutable.HashSet[String]()
    val files = IOUtils.getFiles(directory, ".xml")
    var maxTimeout = 0L

    for(file <- files){

      println("reading: " + file.getPath)
      val(config, timeout, instance, problem, isMax, bestKnown, sols, operators, opStats, opWeights) = readXml(XML.loadFile(file))

      if(timeout > maxTimeout) maxTimeout = timeout

      instances.add(instance)
      if(!instanceTypes.contains(instance)) instanceTypes += instance -> problem
      if(!bks.contains(instance)) bks += instance -> bestKnown
      configs.add(config)

      if(data.contains(instance)){
        if(data(instance)._3.contains(config)) data(instance)._3(config) += sols
        else data(instance)._3 += config -> ArrayBuffer[Seq[(Long, Int)]](sols)
        data(instance)._4 += config -> operators
        if(!data(instance)._5.contains(config)) data(instance)._5 += config -> mutable.Map[String, ArrayBuffer[(Int, Long, Int)]]()
        val opStatsMap = data(instance)._5(config)
        opStats.indices.foreach(i =>{
          if(!opStatsMap.contains(operators(i))) opStatsMap += operators(i) -> ArrayBuffer[(Int, Long, Int)](opStats(i))
          else opStatsMap(operators(i)) += opStats(i)
        })
        data(instance)._6 += config -> opWeights
      }
      else{
        val solsMap = mutable.Map[String, ArrayBuffer[Seq[(Long, Int)]]]()
        solsMap += config -> ArrayBuffer[Seq[(Long, Int)]](sols)
        val opMap = mutable.Map[String, Seq[String]]()
        opMap += config -> operators
        val opStatsMap = mutable.Map[String, mutable.Map[String, ArrayBuffer[(Int, Long, Int)]]]()
        val subOpMap = mutable.Map[String, ArrayBuffer[(Int, Long, Int)]]()
        opStats.indices.foreach(i =>{
          if(!opStatsMap.contains(operators(i))) subOpMap += operators(i) -> ArrayBuffer[(Int, Long, Int)](opStats(i))
          else subOpMap(operators(i)) += opStats(i)
        })
        opStatsMap += config -> subOpMap
        val opScoresMap = mutable.Map[String, Seq[(Long, String, Double)]]()
        opScoresMap += config -> opWeights
        data += instance -> (isMax, bestKnown, solsMap, opMap, opStatsMap, opScoresMap)
      }
    }

    (maxTimeout, instances.toSeq.sorted, configs.toSeq.sorted, instanceTypes, bks, data)
  }

  //Reads an xml config_instance file
  def readXml(content: Node): (String, Long, String, String, Boolean, Option[Int], Seq[(Long, Int)], Seq[String], Seq[(Int, Long, Int)], Seq[(Long, String, Double)]) = {
    val config = (content \ "config").head.text
    val timeout = (content \ "timeout").head.text.toLong
    val instance = (content \ "instance").head.text
    val problem = (content \ "problem").head.text

    val isMax = (content \ "objective_type").head.text match{
      case "max" => true
      case "min" => false
    }

    val bksNode = content \ "best_known_objective"
    val bestKnown = if(bksNode.isEmpty) None else Some(bksNode.head.text.toInt)

    val sols = (content \\ "solution").map(solNode =>(
      (solNode \ "time").head.text.toLong,
      (solNode \ "objective").head.text.toInt
    ))

    val opData = (content \\ "operators").head
    val operators = (opData \\ "operator").map(opNode => (opNode \ "name").head.text.trim).toArray
    val values = Array(10, 30, 70)
    var v = 0
    for(i <- operators.indices){
      if(operators(i).contains("Relax(")){
        operators(i) = operators(i).split("\\(")(0) + "(" + values(v) + ")"
        v += 1
        if(v >= values.length) v = 0
      }
    }
//    println(operators.mkString(","))
    val opStats = (opData \\ "operator").map(opNode => ((opNode \ "execs").head.text.toInt, (opNode \ "time").head.text.toLong, (opNode \ "improvement").head.text.toInt))

    val opWeights = (content \\ "score_update").map(scoreNode =>(
      (scoreNode \ "time").head.text.toLong,
      (scoreNode \ "operator").head.text.trim,
      (scoreNode \ "score").head.text.toDouble
    ))

    (config, timeout, instance, problem, isMax, bestKnown, sols, operators, opStats, opWeights)
  }


  //Aggregates sols by time
  def solsByTime(configs: Seq[String], sols: Seq[(String, ArrayBuffer[Seq[(Long, Int)]])]): Seq[(Long, Array[(Option[Double], Option[Double])])] = {
    val mapping = configs.zipWithIndex.toMap

    val currentSols: Array[(Option[Double], Option[Double])] = Array.fill(configs.length)((None, None))
    val solsByTime = mutable.ArrayBuffer[(Long, Array[(Option[Double], Option[Double])])]()

    sols.flatMap{case (config, runs) => aggregateRuns(runs, config)}
      .sortBy(_._1)
      .foreach{case (time, config, objective, std) =>
        if(currentSols(mapping(config))._1.isEmpty || (objective.isDefined && objective.get < currentSols(mapping(config))._1.get)) {
          currentSols(mapping(config)) = (objective, std)
          if(solsByTime.nonEmpty && solsByTime.last._1 == time) solsByTime.last._2(mapping(config)) = (objective, std)
          else solsByTime += ((time, currentSols.clone()))
        }
      }

    solsByTime.toSeq
  }

  def aggregateRuns(sols: ArrayBuffer[Seq[(Long, Int)]], config: String): Seq[(Long, String, Option[Double], Option[Double])] = {
    println("aggregating: " + config)
    val timeMap = mutable.Map[Long, Array[Option[Int]]]()
    sols.zipWithIndex.foreach{case (solSeq, run) =>
      solSeq.foreach{case (time, objective) =>
        val roundedTime = (time/100000000.0).ceil.toLong
        if(timeMap.contains(roundedTime)){
          val solsArray = timeMap(roundedTime)
          if(solsArray(run).isEmpty || objective < solsArray(run).get) solsArray(run) = Some(objective)
        }
        else{
          val solsArray = Array.tabulate[Option[Int]](sols.length)(i => if(i == run) Some(objective) else None)
          timeMap += roundedTime -> solsArray
        }
      }
    }

    val currentSols: Array[Option[Int]] = Array.fill(sols.length)(None)
    val solsByTime = mutable.ArrayBuffer[(Long, String, Option[Double], Option[Double])]()
    timeMap.toSeq.sortBy(_._1).foreach{case (time, solsOptions) =>
      solsOptions.zipWithIndex.foreach{case (sol, run) =>
        if(sol.isDefined && (currentSols(run).isEmpty || sol.get < currentSols(run).get)) currentSols(run) = sol
      }
      val solsVals = currentSols.filter(_.isDefined).map(_.get)
      val mean = if(solsVals.isEmpty) None else Some(solsVals.sum.toDouble/solsVals.length)
//      val std = if(solsVals.isEmpty) None else Some(Math.sqrt(solsVals.map(v => scala.math.pow(v - mean.get, 2)).sum/solsVals.length))
      val std = if(solsVals.isEmpty) None else Some((Math.sqrt(solsVals.map(v => scala.math.pow(v - mean.get, 2)).sum/solsVals.length)/mean.get)*100.0) //relative
      solsByTime += ((time, config, mean, std))
    }

    solsByTime.toSeq
  }

  //Aggregates operator scores by time
  def opScoresByTime(operators: Seq[String], scores: Seq[(Long, String, Double)]): Seq[(Long, Array[Option[Double]])] = {
    val mapping = operators.zipWithIndex.toMap

    val currentScores: Array[Option[Double]] = Array.fill(operators.length)(None)
    val scoresByTime = mutable.ArrayBuffer[(Long, Array[Option[Double]])]()

//    scores.filter(_._2 != "dummy").sortBy(_._1)
//      .map{case (time, operator, score) => ((time/100000000.0).ceil.toLong, operator, score)}
//      .foreach{case (time, operator, score) =>
//        currentScores(mapping(operator)) = Some(score)
//        if(scoresByTime.nonEmpty && scoresByTime.last._1 == time) scoresByTime.last._2(mapping(operator)) = Some(score)
//        else scoresByTime += ((time, currentScores.clone()))
//      }

    scoresByTime.toSeq
  }

  //Aggreagates gaps by time
  //TODO: make generic method for this and scores by time
  def gapsByTime(
                  instances: Seq[String],
                  configs: Seq[String],
                  gaps: ArrayBuffer[(Long, String, Array[Option[Double]])]
                ): ArrayBuffer[(Long, Array[Array[Option[Double]]])] = {
    val instMapping = instances.zipWithIndex.toMap

    val currentGaps: Array[Array[Option[Double]]] = Array.fill(instances.length, configs.length)(None)
    val gapsByTime = mutable.ArrayBuffer[(Long, Array[Array[Option[Double]]])]()

    gaps.sortBy(_._1).foreach{case (time, instance, gapValues) =>
      val instanceIndex = instMapping(instance)
      gapValues.indices.foreach(configIndex =>{
        currentGaps(instanceIndex)(configIndex) = gapValues(configIndex)
      })
      if(gapsByTime.nonEmpty && gapsByTime.last._1 == time) gapsByTime.last._2(instanceIndex) = currentGaps(instanceIndex).clone
      else gapsByTime += ((time, arrayCopy(currentGaps)))
    }

    gapsByTime
  }

  //Aggregates scores by time
  def scoresByTime(
                    instances: Seq[String],
                    configs: Seq[String],
                    scores: ArrayBuffer[(Long, String, Array[Int])]
                  ): ArrayBuffer[(Long, Array[Array[Int]])] = {
    val instMapping = instances.zipWithIndex.toMap

    val currentScores: Array[Array[Int]] = Array.fill(instances.length, configs.length)(0)
    val scoresByTime = mutable.ArrayBuffer[(Long, Array[Array[Int]])]()

    scores.sortBy(_._1).foreach{case (time, instance, scoreValues) =>
      val instanceIndex = instMapping(instance)
      scoreValues.indices.foreach(configIndex =>{
        currentScores(instanceIndex)(configIndex) = scoreValues(configIndex)
      })
      if(scoresByTime.nonEmpty && scoresByTime.last._1 == time) scoresByTime.last._2(instanceIndex) = currentScores(instanceIndex).clone()
      else scoresByTime += ((time, arrayCopy(currentScores)))
    }

    scoresByTime
  }

  def qualityByTime(
                     instances: Seq[String],
                     configs: Seq[String],
                     quality: ArrayBuffer[(Long, String, Array[Option[Double]])]
                   ): ArrayBuffer[(Long, Array[Array[Option[Double]]])] = {
    val instMapping = instances.zipWithIndex.toMap

    val currentQuality: Array[Array[Option[Double]]] = Array.fill(instances.length, configs.length)(None)
    val qualityByTime = mutable.ArrayBuffer[(Long, Array[Array[Option[Double]]])]()

    quality.sortBy(_._1).foreach{case (time, instance, qualityValues) =>
      val instanceIndex = instMapping(instance)
      qualityValues.indices.foreach(configIndex =>{
        currentQuality(instanceIndex)(configIndex) = qualityValues(configIndex)
      })
      if(qualityByTime.nonEmpty && qualityByTime.last._1 == time) qualityByTime.last._2(instanceIndex) = currentQuality(instanceIndex).clone
      else qualityByTime += ((time, arrayCopy(currentQuality)))
    }

    qualityByTime
  }

  def collapseOpStats(runsStats: Seq[(Int, Long, Int)]): (Double, Double, Double) = {
    val n = runsStats.length
    val (sumE, sumT, sumI) = runsStats.foldLeft((0, 0L, 0)){case((accE, accT, accI), (e, t, i)) => (accE+e, accT+t, accI+i)}
    (sumE.toDouble/n, sumT.toDouble/n, sumI.toDouble/n)
  }

  //Process statistics
  def processStats(
                    instances: Seq[String],
                    configs: Seq[String],
                    data: mutable.Map[String, (Boolean, Option[Int], mutable.Map[String, ArrayBuffer[Seq[(Long, Int)]]], mutable.Map[String, Seq[String]], mutable.Map[String, mutable.Map[String, ArrayBuffer[(Int, Long, Int)]]], mutable.Map[String, Seq[(Long, String, Double)]])],
                    nBks: Int
                  ): (
    Array[Array[(Option[Double], Option[Double], Option[Double])]],
    ArrayBuffer[(Long, Array[Int])],
    ArrayBuffer[(Long, Array[Option[Double]])],
    ArrayBuffer[(Long, Array[Double])],
    Map[String, mutable.Map[String, Array[(String, Double, Double, Double)]]],
    ArrayBuffer[(String, Seq[(Long, Array[Option[Double]])], Seq[(Long, Array[Int])], Option[Seq[(Long, Array[Option[Double]])]], Array[(String, Seq[String], Seq[(Long, Array[Option[Double]])])])]
  ) = {

    val nBests = 3 //Number of best configs to reward (the reward is proportional to the place in the ranking)

    val instMapping = instances.zipWithIndex.toMap //Mapping to the index of each instance
    val confMapping = configs.zipWithIndex.toMap //Mapping to the index of each config

    val scores = mutable.ArrayBuffer[(Long, String, Array[Int])]()
    val gaps = mutable.ArrayBuffer[(Long, String, Array[Option[Double]])]()
    val quality = mutable.ArrayBuffer[(Long, String, Array[Option[Double]])]()
    val bestSols = Array.fill[(Option[Double], Option[Double], Option[Double])](instances.length, configs.length)((None, None, None))
    val opStatsByConfig = mutable.Map[String, mutable.Map[String, Array[(String, Double, Double, Double)]]]()

    // Instance stats: (name, sols, scores, gaps, opScores)
    val instanceStats = mutable.ArrayBuffer[(
      String,
      Seq[(Long, Array[Option[Double]])],
      Seq[(Long, Array[Int])],
      Option[Seq[(Long, Array[Option[Double]])]],
      Array[(String, Seq[String], Seq[(Long, Array[Option[Double]])])]
    )]()

    //Scanning each instance data:
    data.foreach(instanceData => {
      val (name, content) = instanceData
      println("processing: " + name)
      val (isMax, bestKnown, solsFound, operators, opStats, opScores) = content
      println("processing solutions...")
      val solStats = solsByTime(configs, solsFound.toSeq)
      val sortedSols = solStats.map{case (time, sols) => (time, sols.map(_._1))}
      val startSol: Double = if(sortedSols.isEmpty) Double.MaxValue else sortedSols.head._2.map(_.getOrElse(Double.MaxValue)).min

      //Computing gaps:
      println("computing gaps...")
      val instanceGaps: Option[Seq[(Long, Array[Option[Double]])]] = if(bestKnown.isDefined)
        Some(sortedSols.map {case (time, sols) =>
          (time, sols.map {
            case None => None
            case objective: Option[Int] => {
              val diff = objective.get - bestKnown.get //TODO take into account objective type
              Some(diff.toDouble / (startSol - bestKnown.get))
            }
          })
        })
      else None

      //Computing best sols:
      println("computing best sols...")
      if(solStats.nonEmpty){
        val bests = solStats.last._2
        val instanceIndex = instMapping(name)
        configs.indices.foreach(configIndex => bestSols(instanceIndex)(configIndex) = (
          bests(configIndex)._1,
          bests(configIndex)._2,
          if(instanceGaps.isDefined) instanceGaps.get.last._2(configIndex)
          else None
        ))
      }

      //Computing opStats:
      println("computing operator stats...")
      opStats.foreach{case(config, opStatsMap) =>
        val stats = opStatsMap.map{case (opName, runs) =>
          val (e, t, imp) = collapseOpStats(runs.toSeq)
          (opName, e, t, imp)
        }
        if(!opStatsByConfig.contains(config)) opStatsByConfig += config -> mutable.Map[String, Array[(String, Double, Double, Double)]]()
        opStatsByConfig(config) += name -> stats.toArray.filter(_._1.contains("Relax")).sortBy(_._1)
      }

      //Adding gaps:
      if(instanceGaps.isDefined) instanceGaps.get.foreach{case (time, gapValues) => gaps += ((time, name, gapValues))}

      println("computing scores and quality...")
      val instanceScores = ListBuffer[(Long, Array[Int])]()
      sortedSols.foreach{case (time, sols) =>
        //Computing scores:
        val objectives = mutable.Map[Double, mutable.Set[String]]()

        //associating each config to its objective rank:
        sols.indices.foreach(i => if(sols(i).isDefined){
          val objective = sols(i).get
          if(!objectives.contains(objective)) objectives += objective -> mutable.HashSet[String]()
          objectives(objective) += configs(i)
        })

        //Sorting ranks accordingly:
        val rank = (
          if(isMax) objectives.toSeq.sortWith(_._1 > _._1)
          else objectives.toSeq.sortWith(_._1 < _._1)
        ).map(_._2)

        //Computing scores based on best ranks:
        val score = Array.fill[Int](configs.length)(0)
        var i = 0
        while(i < nBests && i < rank.length){
          rank(i).foreach(config => score(confMapping(config)) = nBests - i)
          i+=1
        }

        instanceScores += ((time, score))
        scores += ((time, name, score))

        //Computing quality:
        val bestSol: Option[Double] = sols.min
        if(bestSol.isDefined){
          val solQuality = sols.map(sol => {
            if(sol.isDefined) Some(1 - (sol.get - bestSol.get)/(startSol - bestSol.get))
            else None
          })
          quality += ((time, name, solQuality))
        }
      }

      //Computing operator scores:
      val opScoresByConfig = mutable.ArrayBuffer[(String, Seq[String], Seq[(Long, Array[Option[Double]])])]()
      operators.foreach{case (config, ops) =>
        if(opScores.contains(config)) opScoresByConfig += ((config, ops, opScoresByTime(ops, opScores(config))))
      }

      instanceStats += ((name, sortedSols, instanceScores.toSeq, instanceGaps, opScoresByConfig.toArray))
    })

    //Computing any time gaps:
    val anyTimeGap = gapsByTime(instances, configs, gaps).sortBy(_._1).map{case (time, gapOptions) =>
      (time, configs.indices.map(i =>{
        val gapValues = gapOptions.map(_(i)).filter(_.isDefined).map(_.get)
//        if(gapValues.length == nBks)
          Some(gapValues.sum/gapValues.length).asInstanceOf[Option[Double]]
//        else None
      }).toArray)
    }

    //Computing any time scores:
    val anyTimeScore = scoresByTime(instances, configs, scores).sortBy(_._1).map{case (time, scoreValues) =>
      (time, configs.indices.map(i => {scoreValues.map(_(i)).sum}).toArray)
    }

    //Computing any time quality:
    val anyTimeQuality = qualityByTime(instances, configs, quality).map{case (time, qualityValues) =>
      (time, configs.indices.map(i => {
        val configQuality = qualityValues.map(_(i)).filter(_.isDefined).map(_.get)
        configQuality.sum/configQuality.length
      }).toArray)
    }

    (bestSols, anyTimeScore, anyTimeGap, anyTimeQuality, opStatsByConfig.toMap, instanceStats)
  }

  def renderBestSols(
                      bestSols: Array[Array[(Option[Double], Option[Double], Option[Double])]],
                      configs: Seq[String],
                      instances: Seq[String],
                      instanceTypes: Map[String, String],
                      bestKnownSolutions: Map[String, Option[Int]],
                      showStd: Boolean,
                      showGap: Boolean
                    ): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Instance'", "'Set'", "'Best known solution'") ++ configs.map("'" + _ + "'")
    bestSols.indices.foreach(i => {
      array += Array("'" + instances(i) + "'", "'" + instanceTypes(instances(i)) + "'", bestKnownSolutions(instances(i)) match{
        case None => "null"
        case Some(bks: Int) => bks.toString
      }) ++ bestSols(i).map{
        case (None, _, _) => "null"
        case (Some(bestSol: Double), std, gap) =>
          var str = "'" + bestSol.toString
          if(showStd && std.isDefined) str += ", " + round(std.get, 2).toString
          if(showGap && gap.isDefined) str += ", " + round(gap.get, 2).toString
          str + "'"
      }
    })
    array.toArray
  }


  def renderOpStats(config: String, instStats: mutable.Map[String, Array[(String, Double, Double, Double)]]): Array[Array[String]] = {
    val (instances, opStats) = instStats.toSeq.sortBy(_._1).unzip
    val operators = opStats.head.sortBy(_._1).map(_._1)
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Operator'") ++ instances.map("'" + _ + "'")
    operators.indices.foreach{i => array += Array("'" + operators(i) + "'") ++ opStats.map(_(i)._2.toString)}
    array.toArray
  }

  def renderScoresByTime(scores: Seq[(Long, Array[Int])], configs: Seq[String], timeout: Long, stepped: Boolean = false): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ configs.map("'" + _ + "'")
    scores.foreach{case (time, scoreValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = scoreValues.map(_.toString)
      array += t ++ previous
    }}
    array += Array((timeout/1000000000.0).toString) ++ previous
    array.toArray
  }

  def renderGapsByTime(gaps: Seq[(Long, Array[Option[Double]])], configs: Seq[String], timeout: Long, stepped: Boolean = false): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ configs.map("'" + _ + "'")
    gaps.foreach{case (time, gapValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = gapValues.map{
        case None => "null"
        case Some(gap: Double) => gap.toString
      }
      array += t ++ previous
    }}
    array += Array((timeout/1000000000.0).toString) ++ previous
    array.toArray
  }

  def renderSolsByTime(sols: Seq[(Long, Array[Option[Double]])], configs: Seq[String], timeout: Long, stepped: Boolean = false): Array[Array[String]] ={
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ configs.map("'" + _ + "'")
    sols.foreach{case (time, solValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = solValues.map{
        case None => "null"
        case Some(sol: Double) => sol.toString
      }
      array += t ++ previous
    }}
    array += Array((timeout/1000000000.0).toString) ++ previous
    array.toArray
  }

  def renderQualityByTime(quality: Seq[(Long, Array[Double])], configs: Seq[String], timeout: Long, stepped: Boolean = false): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ configs.map("'" + _ + "'")
    quality.foreach{case (time, qualityValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = qualityValues.map(_.toString)
      array += t ++ previous
    }}
    array += Array((timeout/1000000000.0).toString) ++ previous
    array.toArray
  }

  def renderOpScoresByTime(opScores: Seq[(Long, Array[Option[Double]])], operators: Seq[String], stepped: Boolean = false): Array[Array[String]] ={
    val array = ArrayBuffer[Array[String]]()
    var previous = Array[String]()
    array += Array("'Time'") ++ operators.map("'" + _ + "'")
    opScores.foreach{case (time, scoreValues) => {
      val t = Array((time/10.0).toString)
      if(stepped && previous.nonEmpty) array += t ++ previous
      previous = scoreValues.map{
        case None => "null"
        case Some(score: Double) => score.toString
      }
      array += t ++ previous
    }}
    array.toArray
  }

  private def arrayCopy[T](array: Array[Array[T]])(implicit e: ClassTag[T]): Array[Array[T]] = array.map(_.clone)

  private def round(value: Double, places: Int): Double = BigDecimal(value).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble
}
