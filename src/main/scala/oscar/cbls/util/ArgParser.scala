package oscar.cbls.util

/** Helper to parse command line arguments with flags.
  *
  * Parses arguments in the form: -flag1 value1 -flag2 value2
  *
  * Example usage:
  * {{{
  * val args = Array("-h", "127.0.0.1", "-p", "2551")
  * val parsed = ArgParser.parse(args)
  * val host = parsed.getOrElse("-h", "localhost")
  * val port = parsed.get("-p").map(_.toInt).getOrElse(8080)
  * }}}
  */
object ArgParser {

  /** Parses command line arguments into a Map of flag -> value pairs.
    *
    * @param args
    *   the command line arguments array
    * @return
    *   a Map where keys are flags (including the dash) and values are the corresponding argument
    *   values
    */
  def parse(args: Array[String]): Map[String, String] = {
    args.sliding(2, 2).collect {
      case Array(flag, value) if flag.startsWith("-") => flag -> value
    }.toMap
  }
}
