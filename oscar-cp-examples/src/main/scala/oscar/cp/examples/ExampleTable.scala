package oscar.cp.examples


import oscar.cp._
import oscar.cp.constraints.tables.{InSet,NotEqual,Equal,Less,NotInSet,LessEq,Great,GreatEq,Star}
import oscar.cp.constraints.tables.BasicSmartElement
/**
 * Random example of the usage of all the types of tables constraints
 *
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
object ExampleTable extends CPModel with App {

  // Variables
  val x = Array.fill(5)(CPIntVar(0 to 5))

  // A basic table represented as an array containing the tuple, each tuple being an array of int
  val tableGround: Array[Array[Int]] = Array(
    Array(1, 2, 3),
    Array(2, 3, 4),
    Array(1, 1, 1),
    Array(0, 1, 5),
    Array(1, 2, 5),
    Array(5, 5, 5)
  )

  // A negative table represented as an array containing the tuple, each tuple being an array of int
  // negative table can't contain duplicates
  val tableNeg: Array[Array[Int]] = Array(
    Array(1, 2, 3),
    Array(5, 5, 2),
    Array(2, 5, 0),
    Array(0, 0, 0)
  )

  val star = -1
  // A short table represented as an array containing the tuple, each tuple being an array of int
  // the star being represented by an int outside the domain of each of the variable
  val tableStar: Array[Array[Int]] = Array(
    Array(star, 1, 1),
    Array(2, 5, star),
    Array(3, 5, star),
    Array(1, star, star)
  )

  // A negative short table represented as an array containing the tuple, each tuple being an array of int
  // the star being represented by an int outside the domain of each of the variable
  // No overlap is allowed between tuples in the table
  val tableNegStar: Array[Array[Int]] = Array(
    Array(star, 0, 0),
    Array(star, 1, 2),
    Array(5, 5, star)
  )

  // A basic smart table represented as an array containing the tuple, each tuple being an array of basic smart element
  // The basic smart elements are objects representing the different basic smart primitive (=v,!=v,*,<=v,>=v,\inS,\not\inS)
  val tableBs: Array[Array[BasicSmartElement]] = Array(
    Array(Equal(1), LessEq(2), Star()),
    Array(Less(1), GreatEq(4), NotEqual(2)),
    Array(InSet(Set(1, 3)), NotInSet(Set(0, 5)), Great(2))
  )

  // Constraints
  add(table(Array(x(1), x(2), x(4)), tableGround))
  add(shortTable(Array(x(0),x(2),x(4)),tableStar, star))
  add(negativeTable(Array(x(2),x(3),x(4)),tableNeg))
  add(negativeShortTable(Array(x(1),x(3),x(4)),tableNegStar, star))
  add(basicSmartTable(Array(x(0), x(2), x(4)), tableBs))

  // Search heuristic
  search(binaryFirstFail(x))


  // Execution
  val stats = start()
  println(stats)

}
