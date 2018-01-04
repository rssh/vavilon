package termware

import scala.collection.immutable.Queue

trait MultiTerm {

  def name: Name
  def path: Queue[PointTerm]

  def cardinality: Int
  def multiKind: MultiKind

  def isEmpty: Boolean  = this == EmptyTerm


  def isContradiction: Boolean =
    multiKind match {
      case MultiKind.Contradiction(c) => true
      case _ => false
    }

  def isExist(): Boolean =
    multiKind match {
      case MultiKind.Contradiction(c) => false
      case MultiKind.Empty(e) => false
      case _ => true
    }

  /**
    * Find unificator of this & y and generate unified term with substitution as context
    * @param x = term to substitute
    * @return z = (this unify x):
    *         x.subst(z.context) = this.subst(z.context) = z if one exists,
    *         otherwise - contradiction.
    */
  def unify(x:MultiTerm): MultiTerm

  /**
    * Substitute terms
    * @param x - substitution
    * @return such term,
    */
  def subst(x:MultiTerm): MultiTerm


  def context: MultiTerm
  def uncontext: MultiTerm with EmptyContext


  //TODO: think
 // def internalResolve():MultiTerm

  /**
    * place `this` in context `ctx`
    * if `this` already have context, it must be compatible with new context,
    * otherwise result will be Contradiction,
    */
  def in(ctx:MultiTerm): MultiTerm

  /**
    * place `this` inside context,
    *  if this already have context, it is shallowed by new context.
    */
  def inside(ctx:MultiTerm): MultiTerm

  /**
    * resolve x in context of current term.
    * (i.e. if x->a in context, then return a)
    * @param x
    * @return
    */
  def resolve(x:PointTerm): MultiTerm

  /**
    * narrow x to be compatible with this as context
    * law:  check(narrow(x)) = true.
    */
  def narrow(x:PointTerm):PointTerm

  /**
    * true, if <code>x</code> is compatible with <code>this</code> as context.
    *
    * @param x
    * @return
    */
  def check(x:PointTerm): Boolean

  // apply current term to this.
  //  (semantics - return empty term if not applicable)
  def apply(x:MultiTerm): MultiTerm

  //
  def or(other:MultiTerm):MultiTerm

  def and(other:MultiTerm):MultiTerm // and == unify ?

  def orElse(other: MultiTerm): MultiTerm =
    SeqOrTerm.create(this,other)

  /**
    * create arrow from this to right.
    * @param right - right part of the arrow.
    * @return
    */
  def to(right:MultiTerm):MultiTerm
    = ArrowTerm.create(this,right)

  @inline def ~>(right: MultiTerm):MultiTerm = to(right)

  def lessEq(other:MultiTerm):Boolean =
    (this and other) == this

  def termEq(other:MultiTerm): Boolean =
  {
    this == other
  }



}



trait MultiTermImpl[S <: MultiTermImpl[S]] extends MultiTerm {

    thisMultiTerm: S =>
    //type D
    type Self = S //<: MultiTerm.Aux[D]


}




