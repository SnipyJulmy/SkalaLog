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

  val database = parser.parse(program, new PackratReader[Char](new CharSequenceReader(s))) match {
    case Success(result, _) => result
    case error: NoSuccess => throw new ParserException(error.msg)
  }

  val q = parser.parse(query, new PackratReader[Char](new CharSequenceReader("father(bob)."))) match {
    case Success(result, next) => result
    case error: NoSuccess => throw new ParserException(error.msg)
  }

  println(ask(q, database))

  def ask(query: Query, database: Program): Option[Theta] = {
    // TODO process query additional clauses

    val unifiableWith: List[Clause] = database.clauses
      .filter(_.functor.identifier == query.question.functor.identifier) // proceed just in function of the name
      .filter { clause =>
      clause.head match {
        case Left(a) => a === query.question match {
          case Some(_) => true
          case None => false
        }
        case Right(c) => c === query.question match {
          case Some(_) => true
          case None => false
        }
      }
    }

    if (unifiableWith.isEmpty) {
      println(s"no unifiable clause found for $query in ${database.clauses.mkString("\n")}")
      None
    }
    else {
      println(s"founded clause unifiable with : $unifiableWith")
      ask(query, unifiableWith)
    }
  }

  def ask(query: Query, unifiableWith: List[Clause]): Option[Theta] = unifiableWith match {
    case Nil => None
    case c :: cs => unify(query, c) match {
      case some: Some[Theta] => some
      case None => ask(query, cs)
    }
  }
}
