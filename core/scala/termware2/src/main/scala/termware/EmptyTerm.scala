package termware


final object EmptyTerm extends MultiTerm
{
  override def kind: MultiTermKind = EmptyTermKind

  override def resolved(): MultiTerm = this

  override def resolve(term:MultiTerm): MultiTerm = EmptyTerm

  override def apply(term: PointTerm): MultiTerm = EmptyTerm

  override def contextMerge(otherContext: MultiTerm): MultiTerm = otherContext

  override def context(): MultiTerm = this

  override def subst(context: MultiTerm): MultiTerm = this

  override def unify(term: MultiTerm): MultiTerm = this

}

