package skalalog

import Unification._
import parser._

import scala.util.parsing.input.CharSequenceReader

object ExampleAndTest extends App {

  val parser = new PrologParser {}

  import parser._

  val s =
    """
      |parent(bob,anne).
      |parent(bob,jean).
      |male(bob).
      |male(jean).
      |female(anne).
      |father(A) :- parent(A,_),male(A).
    """.stripMargin

  parser.parse(program, new PackratReader[Char](new CharSequenceReader(s))) match {
    case Success(result, _) => println(result.clauses.mkString("\n"))
    case error: NoSuccess => println(s"error on parsing $s : $error")
  }
}
