package oscar.cbls.api

object Cbls {

  /** Creates an instance of a [[oscar.cbls.api.ApiModel]].
    *
    * @note
    * The [[ApiModel]] class is designed to be used as an
    * [[https://docs.scala-lang.org/scala3/book/ca-context-parameters.html implicit value]]. In
    * our context, an implicit value is similar to a default value. This makes the code less
    * verbose and allows us to use
    * [[https://docs.scala-lang.org/scala3/book/ca-implicit-conversions.html implicit conversions]]
    * (see [[oscar.cbls.examples.WLPAdvancedModelingExample]]). <br>
    *
    * '''WARNING:''' Don't use implicit values if you are working with multiple models. This can
    * lead to errors. Use a syntax similar to [[oscar.cbls.examples.WLPBeginnerModelingExample]].
    * @param name
    *   the name of the model
    * @param debugLevel
    *   debug level of associated [[oscar.cbls.core.computation.Store]]
    */
  def model(name: String, debugLevel: Int = 0): ApiModel = ApiModel(name, debugLevel)
}
