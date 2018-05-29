package termware


final object EmptyTerm extends MultiTerm
{
  override def kind: MultiTermKind = EmptyTermKind
}

