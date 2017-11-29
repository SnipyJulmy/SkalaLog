package skalalog.parser

import skalalog.NoFunctorException

import scala.collection.immutable

/* Grammar representation of SkalaLog */

sealed trait Term {
  def functor: Atom = this match {
    case atom: Atom => atom
    case Variable(identifier) => throw new NoFunctorException(s"no functor for variable $identifier")
    case CompoundTerm(atom, _) => atom
    case _: Number => throw new NoFunctorException(s"number don't have functor")
    case _: PrologList => Atom(".")
    case _: PrologString => Atom(".")
    case _: PrologSequence => Atom(",")
  }

  def arity: Int = this match {
    case _: Atom => 0
    case _: Variable => 0
    case CompoundTerm(_, args) => args.length
  }
}

case class Atom(identifier: String) extends Term
case class Variable(identifier: String) extends Term
case class CompoundTerm(atom: Atom, args: List[Term]) extends Term

// Number
sealed trait Number extends Term
case class IntNumber(value: Int) extends Number
case class DoubleNumber(value: Double) extends Number

// List
sealed trait PrologList extends Term {
  def toList: List[Term] = this match {
    case EmptyList => Nil
    case TermList(elements, end) => end match {
      case Left(value) => elements ::: value.toList
      case Right(value) => elements ::: (value :: Nil)
    }
  }
}
case object EmptyList extends PrologList
case class TermList(elements: List[Term], end: Either[PrologList, Variable]) extends PrologList

// String
case class PrologString(value: String) extends Term

// Sequence
case class PrologSequence(left: Term, right: Term) extends Term {
  def toList: List[Term] = right match {
    case s: PrologSequence => left :: s.toList
    case _ => left :: right :: Nil
  }
}

// Program and other key component
case class Program(clauses: List[Clause])

sealed trait Clause {

  val head: Either[Atom, CompoundTerm]

  val arity: Int = head match {
    case Left(_) => 0 // Atom
    case Right(compoundTerm) => compoundTerm.arity
  }

  val functor: Atom = head match {
    case Left(value) => value
    case Right(value) => value.atom
  }
}

case class Rule(head: Either[Atom, CompoundTerm], body: List[Term]) extends Clause
case class Predicate(head: Either[Atom, CompoundTerm]) extends Clause

case class Query(clauses: List[Clause], question: Term)