package termware

import cats.effect.IO

trait MultiTerm
{

  /**
    * Kind of term.
    * Used for marching by cases of kind, when we want form compiler exhausive case analysis)
    *  (this is workaround for restricting case analysis to sealed traits in scala)
    * @return kind of term (sealed case class)
    */
  def kind:MultiTermKind


  /**
    * Term, resolved in own context.
    * @return
    */
  def resolved(): MultiTerm

  /**
    * resolve term in context of this
    * @param term
    * @return
    */
  def resolve(term:MultiTerm): MultiTerm

  /**
    * Apply term to this in current context.
    * Form expression (this term) and then apply reduction rules.
    * By condition, return emtpy if term was not applicable to this.
    * @param term
    * @return
    */
  def apply(term:PointTerm): MultiTerm


  /**
    * Substitute term in context
    * @param context in which we substitute one.
    * @return
    */
  def subst(context:MultiTerm):MultiTerm

  /**
    * Unify
    * @return
    */
  def unify(arg: TermInContext): TermInContext

  @inline
  final def <> (arg: TermInContext): TermInContext = {
    this unify arg
  }

  @inline
  final def ^^ (context:MultiTerm): TermInContext =
    TermInContext(this,context)

  def isEmpty(): Boolean = (kind == EmptyTermKind)

  def isContradiction(): Boolean = (kind == ContradictionTermKind)

  def isExists(): Boolean = !( isEmpty() || isContradiction())

  def ifExists(t:MultiTerm): MultiTerm  =
    if (t.isEmpty() || t.isContradiction()) {
      t
    } else this

  def or(x:MultiTerm): MultiTerm

  def and(x:MultiTerm): MultiTerm = {
    val u = unify(TermInContext(x,EmptyTerm))
    u.term.subst(u.context)
  }



  def compatibleOr(x:MultiTerm): MultiTerm

}

