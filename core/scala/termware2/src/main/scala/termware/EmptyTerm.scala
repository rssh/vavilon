package termware

import termware.util.FastRefOption


final object EmptyTerm extends MultiTerm  with SetTerm
{
  override def kind: MultiTermKind = EmptyTermKind

  override lazy val resolved: MultiTerm = this

  override def resolve(term:MultiTerm): MultiTerm = EmptyTerm

  //TODO: rethink
  override def apply(term: PointTerm): MultiTerm = EmptyTerm

  def context(): MultiTerm = this

  override def subst(context: MultiTerm): MultiTerm = this

  override def unify(x: TermInContext): TermInContext = TermInContext.empty

  override def or(x: MultiTerm): MultiTerm = x

  override def and(x: MultiTerm): MultiTerm = this

  override def compatibleOr(x: MultiTerm): MultiTerm = x

  override def applyOne(term: PointTerm): MultiTerm = apply(term)

  override def applyAll(term: PointTerm): MultiTerm = apply(term)

  override def selectOne(pattern: TermInContext): TermInContext = TermInContext(this,pattern.context)

  override def selectAll(pattern: TermInContext): Seq[TermInContext] = Seq.empty

  override def mapReduce[A](map: MultiTerm => A)(reduce: (A, A) => A)(zero: =>A) = zero

  override def members(): Seq[PointTerm] = Seq.empty
}

object IsEmptyTerm
{

  def unapply(x:MultiTerm):FastRefOption[EmptyTerm.type]={
    if (x==EmptyTerm) FastRefOption(EmptyTerm) else FastRefOption.empty
  }

}
