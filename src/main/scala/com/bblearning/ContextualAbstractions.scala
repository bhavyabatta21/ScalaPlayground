package com.bblearning

object ContextualAbstractions {

  /*
      1 - Context parameters/arguments
   */
  val aList = List(2, 1, 3, 4)
  val anOrderedList = aList.sorted // Contextual argument: (descendingOrdering)

  // Ordering
  given descendingOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _) // (a, b) => a > b

  // analogous to an implicit val
  trait Combinator[A] {
    def combine(x: A, y: A): A
  }

  def combineAll[A](list: List[A])(using combinator: Combinator[A]): A =
    list.reduce((a, b) => combinator.combine(a, b))

  given intCombinator: Combinator[Int] with {
    override def combine(x: Int, y: Int): Int = x + y
  }

  val theSum = combineAll(aList) // Uses given intCombinator

  /*
      Given places:
      - Local scope
      - Imported scope
      - The companions of all the types involved in the call
        - Companion of List
        - Companion of Int
   */

  // Context bounds
  def combineAll_v2[A](list: List[A])(using Combinator[A]): A =
    combineAll(list)

  def combineAll_v3[A: Combinator](list: List[A]): A =
    combineAll(list)

  /*
      Where context args are useful:
      - Type classes
      - Dependency injection
      - Context-dependent functionality
      - Type-level programming
   */

  /*
      2 - Extension methods
   */

  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name, I love Scala!"
  }

  extension (string: String)
    def greet(): String = Person(string).greet()

  val danielsGreeting = "Daniel".greet() // "Type enrichment"

  // POWER
  extension [A](list: List[A])
    def combineAllValues(using combinator: Combinator[A]): A =
      list.reduce(combinator.combine)

  val theSum_v2 = aList.combineAllValues

  def main(args: Array[String]): Unit = {
    println(anOrderedList)
    println(theSum)
    println(theSum_v2)
  }
}