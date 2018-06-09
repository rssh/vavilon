package termware


final object EmptyTerm extends MultiTerm
{
  override def kind: MultiTermKind = EmptyTermKind

  override def resolved(): MultiTerm = this

  override def resolve(term:MultiTerm): MultiTerm = EmptyTerm

  override def apply(term: PointTerm): MultiTerm = EmptyTerm

}

