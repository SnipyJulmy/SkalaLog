package skalalog

// Simple prolog grammar translated to Scala trait and case class
object Grammar {

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

}
