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
    * Apply term to this.  Form expression (this term) and then apply reduction rules.
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
    * @param term
    * @return
    */
  def unify(term:MultiTerm):MultiTerm

  @inline
  final def <> (term:MultiTerm): MultiTerm = unify(term)

  /**
    * @return context of this term (empty term, if context is not set)
    */
  def context(): MultiTerm

  /**
    * Merge context of this term with otherContext.
    * Note, that context should be compatible with existing.
    * @return term in new context
    */
  def contextMerge(otherContext:MultiTerm): MultiTerm

  def isEmpty(): Boolean = (kind == EmptyTermKind)

  def isContradiction(): Boolean = (kind == ContradictionTermKind)

  def ifExists(t:MultiTerm): MultiTerm  =
    if (t.isEmpty() || t.isContradiction()) {
      t
    } else this

}

