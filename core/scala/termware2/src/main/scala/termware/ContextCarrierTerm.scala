package termware

trait ContextCarrierTerm extends PointTerm {

  def context(): MultiTerm

override def resolve(term: MultiTerm): MultiTerm =
  term.kind match {
    case x: PointTermKind => context.apply(x.pointTerm(term))
    case x: EmptyTermKind => EmptyTerm

      // TODO:  select or from all contexts ?
    case x: StarTermKind => EmptyTerm

    case x: ContradictionTermKind => term
    case x: SetTermKind =>
       x.set(term).mapReduce(x => this.resolve(x))(_ or _)(EmptyTerm)
    case x: OrElseTermKind =>
      x.cast(term).firstNotEmpty(_.resolve(term))
  }


}
