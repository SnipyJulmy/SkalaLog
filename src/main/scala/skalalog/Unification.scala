package skalalog

import parser._

object Unification {

  // type definition for theta, which is basically a map from variable to term...
  type Theta = Map[Variable, Term]

  def unify(a: Term, b: Term): Option[Theta] = {
    unify(a, b, Map())
  }

  def unify(a: Term, b: Term, theta: Theta): Option[Theta] = {
    val t1 = find(a, theta)
    val t2 = find(b, theta)
    if (t1 == t2)
      Some(theta)
    else (a, b) match {
      case (vA: Variable, vB: Variable) =>
        Some(theta + (vA -> vB))
      case (Atom(idA), Atom(idB)) =>
        if (idA != idB) None
        else Some(theta)
      case (variable: Variable, term: Term) =>
        if (term.contains(variable, theta)) None
        else Some(theta + (variable -> term))
      case (term: Term, variable: Variable) =>
        if (term.contains(variable, theta)) None
        else Some(theta + (variable -> term))
      case (CompoundTerm(functorLeft, argsLeft), CompoundTerm(functorRight, argsRight)) =>
        if (functorLeft == functorRight)
          unifyList(argsLeft, argsRight, theta)
        else None
    }
  }

  def unifyList(left: List[Term], right: List[Term], theta: Theta): Option[Theta] = {
    if (left.length != right.length) None
    else (left, right) match {
      case (Nil, Nil) => Some(theta)
      case (Nil, _) => None
      case (_, Nil) => None
      case (x :: xs, y :: ys) =>
        unify(x, y, theta) match {
          case Some(newTheta) => unifyList(xs, ys, newTheta)
          case None => None
        }
    }
  }

  def find(term: Term, theta: Theta): Term = term match {
    case variable: Variable =>
      theta.get(variable) match {
        case Some(value) => value
        case None => term
      }
    case _ => term
  }

  implicit class UnifiableTerm(term: Term) {
    def ===(that: Term): Option[Theta] = unify(term, that)

    def contains(variable: Variable, theta: Theta): Boolean = term match {
      case _: Atom => false
      case selfV: Variable => selfV == variable
      case CompoundTerm(_, args) => args.contains(variable, theta)
    }
  }

  implicit class UnifiableTermList(terms: List[Term]) {
    def contains(variable: Variable, theta: Theta): Boolean = terms match {
      case Nil => false
      case x :: xs =>
        if (find(x, theta).contains(variable, theta)) true
        else xs.contains(variable, theta)
    }
  }

}
