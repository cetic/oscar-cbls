package oscar.cp.examples

import oscar.cp._

import scala.collection.SortedSet

/**
 * Problem statement :
 * There are 13 words : BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, JUNK, LIMN, QUIP, SWAG, VISA, WISH
 * 24 different letters that appear in the 13 words.
 * Assign the 24 letters appearing to 4 different cubes (one letter/face) so that the 4 letters of each word appears on different cubes.
 * @author Pierre Schaus pschaus@gmail.com
 */
object Cubes extends CPModel with App {

  val numCubes = 4
  val numFaces = 6

  val words = "BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, JUNK, LIMN, QUIP, SWAG, VISA, WISH".split(", ")
  val letters = words.foldLeft(SortedSet.empty[Char]) { _ ++ _ }.toSeq // Set of all 24 letters
  val numLetters = letters.size
  def letterToInt(letter: Char): Int = letters.indexOf(letter) // Letter from letter index

  val placement = Array.fill(numLetters)(CPIntVar(0 until numCubes)) // The cube (0 to 3) on which each letter is placed

  onSolution {
    for (cube <- 0 until numCubes) {
      val values = placement.zipWithIndex.map{ case (v, i) => if (v.value == cube) letters(i) else "."}.mkString(" ")
      println(s"Cube $cube: $values")
    }
    println
  }

  add(gcc(placement, 0 until numCubes, numFaces, numFaces), Strong) // There must be exactly 6 letters on each cube

  // The 4 letters of each word must be placed on different cubes
  for (word <- words) {
    add(allDifferent(word.map(l =>placement(letterToInt(l)))), Strong)
  }

  search { binaryStatic(placement) }

  val stats = start()

  println(stats)
}
