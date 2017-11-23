package skalalog.parser

/* Grammar representation of SkalaLog */

sealed trait Term {

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
case class FloatNumber(value: Double) extends Number

// List
sealed trait PrologList extends Term
case object EmptyList extends PrologList
case class TermList(head: Term, tail: PrologList) extends PrologList

// String
case class PrologString(value: String) extends Term with PrologList

// Sequence
case class PrologSequence(left: Term, right: Term)

// Program and other key component
case class Program(clauses: List[Clause])
sealed trait Clause
case class Predicate(head: Either[Atom, CompoundTerm], body: PrologSequence) extends Clause
case class Rule(head: Either[Atom, CompoundTerm]) extends Clause

case class Query(predicates: List[Predicate])