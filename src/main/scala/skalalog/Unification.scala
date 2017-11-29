package skalalog

import parser._

object Unification {

  // type definition for theta, which is basically a map from variable to term...
  type Theta = Map[Variable, Term]

  val emptyTheta: Theta = Map[Variable, Term]()

  def ask(query: Query)(implicit database: Program): Option[Theta] = {
    if (query.clauses.isEmpty) {
      askWithoutAdditionalClause(query)
    } else {
      // add the additional clauses to the database for processing
      // TODO : check if this is working
      ask(query.copy(clauses = query.clauses.tail))(database.copy(query.clauses.head :: database.clauses))
    }
  }

  private def askWithoutAdditionalClause(query: Query)(implicit database: Program): Option[Theta] = {
    val clauses = findClauses(query.question)
    if (clauses.isEmpty) None
    else {
      // we proceed the clause in order they appear (very important !)
      // For the moment we are only interested in find the truth about a predicate
      // (does it solve or not) and not interested in finding all solutions and so on...
      // (we do it later)
      inner(clauses)
    }

    def inner(list: List[Clause]): Option[Theta] = list match {
      case Nil => None
      case c :: cs => processClause(query, c, emptyTheta) match {
        case someTheta: Some[Theta] => someTheta
        case None => inner(cs)
      }
    }
  }

  private def processClause(query: Query, clause: Clause, theta: Theta)(implicit database: Program): Option[Theta] = {
    clause match {
      case rule: Rule => processRule(query, rule, theta)
      case predicate: Predicate => processPredicate(query, predicate, theta)
    }
  }

  private def processRule(query: Query, rule: Rule, theta: Theta)(implicit database: Program): Option[Theta] = {
    ???
  }

  private def processPredicate(query: Query, predicate: Predicate, theta: Theta)(implicit database: Program): Option[Theta] = {
    ???
  }

  /**
    * Find the clauses of the current database that can be unified with term
    *
    * @param term     the Term to unify with potential clauses of the database
    * @param database the current knowledge base of the interpreter
    * @return all the clauses that can unify with term
    */
  private def findClauses(term: Term)(implicit database: Program): List[Clause] = {
    database.clauses.filter { clause =>
      clause.arity == term.arity && clause.functor == term.functor
    }
  }

  def unify(term: Term, clause: Clause, theta: Theta)(implicit database: Program): Option[Theta] = {
    ???
  }

  /*
  // unify a term with a predicate, which is just the unification with the head of the predicate
  def unify(term: Term, predicate: Predicate, theta: Theta)(implicit database: Program): Option[Theta] = {
    predicate.head match {
      case Left(atom) => unify(term, atom, theta)
      case Right(compoundTerm) => unify(term, compoundTerm, theta)
    }
  }

  // unify a term with a rule
  // - first we have to unify the term with the head of the rule
  // - then we use theta to unify the rest of the rules (body)
  def unify(term: Term, rule: Rule, theta: Theta)(implicit database: Program): Option[Theta] = ???
  */

  def unify(a: Term, b: Term): Option[Theta] = {
    unify(a, b, Map[Variable, Term]())
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

  def unify(q: Term, body: List[Term], theta: Theta): Option[Theta] = body match {
    case Nil => Some(theta)
    case t :: ts => unify(q, t, theta) match {
      case Some(newTheta) => unify(q, ts, newTheta)
      case None => None
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
