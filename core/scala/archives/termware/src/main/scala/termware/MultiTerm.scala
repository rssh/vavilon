package termware

import termware.missing.RefOrNull

import scala.collection.immutable.Queue

trait MultiTerm {

  def name: Name

  def nameTerm: PointTerm

  def path: Queue[PointTerm]

  def cardinality: Int

  def multiKind: MultiKind

  def isEmpty: Boolean  = this == EmptyTerm

  def subterm(n: Name): MultiTerm

  def subterm(i:Int): MultiTerm

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


  /**
    * place `this` in context `ctx`
    * if `this` already have context, it must be compatible with new context,
    * otherwise result will be Contradiction,
    */
  def in(ctx:MultiTerm): MultiTerm

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

  /**
    * select multiterms, compatibe to context
    * @param x
    * @return
    */
  def selectCompatible(x:MultiTerm): MultiTerm =
    x.multiKind match {
      case MultiKind.Empty(e) => e
      case MultiKind.Contradiction(ct) => ct
      case MultiKind.Star(s) => StarTerm(s.context.and(x.context))
      case MultiKind.Set(s) => s.mapOr(_.selectCompatible(x))
      case MultiKind.SeqOr(s) =>
    }

  // apply current term to this.
  //  (semantics - return empty term if not applicable)
  def apply(x:MultiTerm): MultiTerm


  //
  def or(other:MultiTerm):MultiTerm

  def and(other:MultiTerm):MultiTerm // and == unify ?

  def orElse(other: MultiTerm): MultiTerm =
    SeqOrTerm.create(this,other)

  def select(head: MultiTerm): MultiTerm = and(head ~> DefaultStarTerm)

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



object MultiTerm
{

  object IsEmpty{
    def unapply(t: MultiTerm): RefOrNull[MultiTerm] =
      if (t.isEmpty) new RefOrNull[MultiTerm](null) else new RefOrNull[MultiTerm](t)
  }

  object NonEmpty{
    def unapply(t: MultiTerm): RefOrNull[MultiTerm] =
      if (t.isEmpty) new RefOrNull[MultiTerm](null) else new RefOrNull(t)
  }


}


trait MultiTermImpl[S <: MultiTermImpl[S]] extends MultiTerm {

    thisMultiTerm: S =>

    type Self <: MultiTermImpl[S]

}





