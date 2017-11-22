package skalalog

import Grammar._
import Unification._

object ExampleAndTest extends App {
  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val A = Variable("A")
  val B = Variable("B")

  val parent1 = CompoundTerm(Atom("parent"),List(a,b))
  val parent2 = CompoundTerm(Atom("parent"),List(a,A))

  println(parent1 === parent2)
}
