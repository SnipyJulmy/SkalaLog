package skalalog.parser

import skalalog.NoFunctorException

import scala.annotation.tailrec

/* Grammar representation of SkalaLog */

sealed trait Term {

  // return the list of all the variables contained in a Term
  lazy val variables: List[Variable] = {

    def inner(acc: Set[Variable]): Set[Variable] = this match {
      case _: Atom => acc
      case variable: Variable => acc + variable
      case CompoundTerm(_, args) =>
        args.map(_.variables).foldLeft(Set[Variable]())((elt, acc) => elt ++ acc)
      case _: Number => acc
      case pl: PrologList => pl match {
        case EmptyList => acc
        case TermList(elements, end) =>
          val vars = elements.map(_.variables).foldLeft(Set[Variable]())((elt, acc) => elt ++ acc)
          end match {
            case Left(prologList) => vars ++ prologList.variables
            case Right(variable) => vars + variable
          }

      }
      case PrologString(value) => acc
      case PrologSequence(left, right) => acc ++ left.variables ++ right.variables
    }

    inner(Set[Variable]()).toList
  }

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
    case _: Number => 0
    case prologList: PrologList => prologList match {
      case EmptyList => 0
      case TermList(elements, end) => end match {
        case Left(pList) => elements.length + pList.arity
        case Right(_) => elements.length
      }
    }
    case PrologString(value) => value.length
    case PrologSequence(_, right) => 1 + right.arity
  }

  override def toString: String = this match {
    case Atom(identifier) => identifier
    case Variable(identifier) => identifier
    case CompoundTerm(atom, args) => s"$atom(${args.map(_.toString).mkString(",")})"
    case n: Number => n match {
      case IntNumber(value) => s"$value"
      case DoubleNumber(value) => s"$value"
    }
    case l: PrologList => l match {
      case EmptyList => s"[]"
      case TermList(elements, end) => s"[${elements.map(_.toString).mkString(",")}|${
        end match {
          case Left(prologList) => prologList.toString
          case Right(variable) => variable.toString
        }
      }]"
    }
    case PrologString(value) => value
    case PrologSequence(left, right) => s"','($left,$right)"
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

  val headTerm: Term = head match {
    case Left(atom) => atom
    case Right(compoundTerm) => compoundTerm
  }
}

case class Rule(head: Either[Atom, CompoundTerm], body: List[Term]) extends Clause
case class Predicate(head: Either[Atom, CompoundTerm]) extends Clause

case class Query(clauses: List[Clause], question: Term)