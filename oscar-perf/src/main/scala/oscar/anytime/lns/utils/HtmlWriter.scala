package oscar.anytime.lns.utils

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex

object HtmlWriter{

  // Flags used in the template file to indicate where to insert the information to display:
  val DataFlag: Regex = """\s*/\*<data>\*/\s*""".r
  val DocumentFlag: Regex = """\s*<!--<document>-->\s*""".r

  /**
    * Formats an array of arrays to the javascript array representation.
    * @param table the array to format
    */
  def tableToHtmlString[T](table: Array[Array[T]]): String =
    "[\n" + table.map(row => "[" + row.mkString(", ") + "]").mkString(",\n") + "\n]"
}

/**
  * Tool class used to create a html report based on a given template.
  *
  * Usage: once the instance created by linking a template file and an output file, elements are added using the
  * appropriate functions. Once all the elements are added, the writeToHtml function writes the final document
  * into the output file. the elements are displayed in their addition order.
  */
class HtmlWriter(templatePath: String, outputPath: String){

  // the data elements to insert in the drawing javascript function. Format: (id, element type, data source, options):
  val data: mutable.ArrayBuffer[(String, String, String, String)] = new ArrayBuffer[(String, String, String, String)]()

  // The html markers to insert in the body part. Format: (marker type, text)
  val document: mutable.ArrayBuffer[(String, String)] = new ArrayBuffer[(String, String)]()

  private var defaultId = 0

  /**
    * Adds a heading to the html document.
    * @param text the text of the heading.
    */
  def addHeading(text: String, level: Int = 1): Unit = {
    if(level == 2) document += (("sub-heading", text))
    else if(level == 3) document += (("sub-sub-heading", text))
    else document += (("heading", text))
  }

  /**
    * Adds a paragraph to the html document.
    * @param text the text of the paragraph.
    */
  def addParagraph(text: String): Unit = {
    document += (("paragraph", text))
  }

  /**
    * Adds a data element to the html document.
    * @param elemType the type of the element added.
    * @param title the title to display above the element.
    * @param src the data source of the element (a string correctly formatted).
    * @param id (optional - if none a default id will be given) the id of the element.
    */
  def addElement(elemType: String, title:String, src:String, id:String = getNextDefaultId): Unit = {
    val options = elemType match{
      case "line" => "{\ntitle: '" + title + "',\nchartType:'linear',\nhAxis: {minValue: 0}\n}"
      case "logline" => "{\ntitle: '" + title + "',\nchartType:'logarithmic',\nhAxis: {minValue: 0}\n}"
      case "stacked bar" => "{\ntitle: '" + title + "',\nisStacked:'percent',\n}"
      case _ => "{\ntitle: '" + title + "'\n}"
    }
    document += (("elem", id))
    data += ((id, elemType, src, options))
  }

  /**
    * Writes the final html document in the ouput file.
    */
  def writeToHtml(): Unit = {

    //Creating and opening output file:
    val output = new File(outputPath)
    output.getParentFile.mkdirs()
    output.createNewFile()
    val writer = new BufferedWriter(new FileWriter(output))

    //Reading and copying template file:
    Source.fromFile(templatePath).getLines().foreach {

      //If data insertion point, writing data:
      case HtmlWriter.DataFlag(_*) => writer.write(renderData() + "\n")

      //If document insertion point, writing document:
      case HtmlWriter.DocumentFlag(_*) => writer.write(renderDocument() + "\n")

      //Else copying line:
      case l: String => writer.write(l + "\n")
    }

    //Closing output file:
    writer.close()
  }

  private def renderData(): String = data
    .map(tuple => "[\n'" + tuple._1 + "',\n'" + tuple._2 + "',\n" + tuple._3 + ",\n" + tuple._4 + "\n]")
    .mkString(",\n")

  private def renderDocument(): String = document.map{
      case ("heading", text) => "<h1>" + text + "</h1>"
      case ("sub-heading", text) => "<h2>" + text + "</h2>"
      case ("sub-sub-heading", text) => "<h3>" + text + "</h3>"
      case ("paragraph", text) => "<p>" + text + "</p>"
      case ("elem", id) => "<button onclick=\"display(\'" + id + "\')\">Display</button>\n<div id=" + id + "></div>"
    }
    .mkString("\n")

  private def getNextDefaultId: String = {
    val id = "elem_" + defaultId
    defaultId += 1
    id
  }
}
