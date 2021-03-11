package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, Strong, add, allDifferent, binaryFirstFail, onSolution, search, start}

/**
 * Problem statement :
 * One evening there was a murder in the home of married couple, their son and daughter.
 * One of these four people murdered one of the others.
 * One of the members of the family witnessed the crime.
 * The other one helped the murderer.
 * These are the things we know for sure:
 *
 * 1. The witness and the one who helped the murderer were not of the same sex.
 * 2. The oldest person and the witness were not of the same sex.
 * 3. The youngest person and the victim were not of the same sex.
 * 4. The one who helped the murderer was older than the victim.
 * 5. The father was the oldest member of the family.
 * 6. The murderer was not the youngest member of the family.
 *
 * Who was the murderer?
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object MurderMystery extends CPModel with App {

  // data
  val Array(son, daughter, father, mother) = (0 to 3).toArray
  val name = Array("son", "daughter", "father", "mother")
  val sex = Array(0, 1, 0, 1) // 0 = male, 1 = female

  // variables
  val personWithAge = Array.fill(4)(CPIntVar(0 to 3)) // personWithAge(i) is younger than personWithAge(i+1)
  val age = Array.fill(4)(CPIntVar(0 to 3)) // age(i) is the age of person i
  val Array(murderer, witness, helper, victim) = Array.fill(4)(CPIntVar(0 to 3))
  val oldest = personWithAge(3)
  val youngest = personWithAge(0)

  add(allDifferent(Array(murderer, witness, helper, victim)), Strong)
  add(allDifferent(age), Strong)

  // 1. The witness and the one who helped the murderer were not of the same sex.
  add(sex(witness) !== sex(helper))

  // 2. The oldest person and the witness were not of the same sex.
  add(sex(oldest) !== sex(witness))

  // 3. The youngest person and the victim were not of the same sex.
  add(sex(youngest) !== sex(victim))

  // 4. The one who helped the murderer was older than the victim.
  for (i <- 0 to 3) {
    add(age(personWithAge(i)) === i)
  }
  add(age(helper) > age(victim))

  // 5. The father was the oldest member of the family.
  add(oldest === father)
  add(personWithAge(2) === mother)

  // 6. The murderer was not the youngest member of the family.
  add(youngest !== murderer)

  search {
    binaryFirstFail(Array(murderer, witness, helper, victim))
  }

  onSolution {
    println("murderer:" + name(murderer.value) + " witness:" + name(witness.value) + " helper:" + name(helper.value) + " victim:" + name(victim.value) + " youngest:" + name(youngest.value))
  }

  val stats = start()
  println(stats)
}
