package skalalog

import parser._
import skalalog.Unification._

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

  val queryString = "father(A)."

  implicit val database: Program = parser.parse(program, new PackratReader[Char](new CharSequenceReader(s))) match {
    case Success(result, _) => result
    case error: NoSuccess => throw new ParserException(error.msg)
  }

  val q = parser.parse(query, new PackratReader[Char](new CharSequenceReader(queryString))) match {
    case Success(result, next) => result
    case error: NoSuccess => throw new ParserException(error.msg)
  }

  println(ask(q))
}
