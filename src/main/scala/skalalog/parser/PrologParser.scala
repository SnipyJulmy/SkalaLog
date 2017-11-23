package skalalog.parser

import skalalog.NoListFoundException

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

trait PrologParser extends RegexParsers with PackratParsers {

  override protected val whiteSpace: Regex = "[ \t\r\f\n]".r

  override def skipWhitespace: Boolean = true

  lazy val program: Parser[Program] = rep(clause) ^^ { clauses => Program(clauses) }

  lazy val clause: Parser[Clause] = {
    headClause <~ "." ^^ { h => Predicate(h) } |
      headClause ~ (":-" ~> bodyClause) <~ "." ^^ { case h ~ b => Rule(h, b) }
  }

  lazy val headClause: Parser[Either[Atom, CompoundTerm]] = {
    compoundTerm ^^ {
      Right(_)
    } |
      atom ^^ {
        Left(_)
      }
  }

  lazy val bodyClause: Parser[List[Term]] = term ~ rep("," ~> term) ^^ { case h ~ t => h :: t }

  lazy val term: Parser[Term] = compoundTerm | number | prologList | prologSequence | atom | variable
  lazy val atom: Parser[Atom] = "[a-z][a-zA-Z1-9_]*".r ^^ Atom
  lazy val variable: Parser[Variable] = "[A-Z_][a-zA-Z1-9_]*".r ^^ Variable
  lazy val compoundTerm: Parser[CompoundTerm] = atom ~ termArgs ^^ { case a ~ args => CompoundTerm(a, args) }

  lazy val termArgs: Parser[List[Term]] = lp ~> term ~ rep("," ~> term) <~ rp ^^ { case h ~ t => h :: t }
  lazy val number: Parser[Number] = intNumber | floatNumber
  lazy val intNumber: Parser[IntNumber] = "[+-]?([0-9]+)".r ^^ { ns => IntNumber(ns.toInt) }
  lazy val floatNumber: Parser[DoubleNumber] = "[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)(E[0-9]+)?".r ^^ {
    ns => DoubleNumber(ns.toDouble)
  }


  lazy val prologList: Parser[PrologList] = {
    lb ~ rb ^^^ EmptyList |
      lb ~> term ~ rep("," ~> term) <~ rb ^^ { case h ~ t => TermList(h :: t, Left(EmptyList)) } |
      lb ~> term ~ rep("," ~> term) ~ ("|" ~> prologList) <~ rb ^^ {
        case h ~ t ~ r => TermList((h :: t) ::: r.toList, Left(EmptyList))
      } |
      lb ~> term ~ rep("," ~> term) ~ ("|" ~> (prologList | variable)) <~ rb ^^ {
        case h ~ t ~ r => r match {
          case variable: Variable => TermList(h :: t, Right(variable))
          case list: PrologList => TermList(h :: t, Left(list))
          case _ => throw new NoListFoundException(s"$r is not a list")
        }
      }
  }

  lazy val prologSequence: Parser[PrologSequence] = {
    lp ~> (term ~ ("," ~> term)) <~ rp ^^ { case t1 ~ t2 => PrologSequence(t1, t2) }
  }

  lazy val lp = "("
  lazy val rp = ")"
  lazy val lcb = "{"
  lazy val rcb = "}"
  lazy val lb = "["
  lazy val rb = "]"
}
