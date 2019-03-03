package termware


/**
  * Term which carry internal context.
  */
trait ContextCarrierTerm extends PointTermOps {

  this: PointTerm =>

  def context(): MultiTerm


  override def resolve(term: MultiTerm): MultiTerm =
  term.kind match {
    case x: PointTermKind => context.termApply(x.pointTerm(term))
    case x: EmptyTermKind => EmptyTerm

      // TODO:  select or from all contexts ?
    case x: StarTermKind => EmptyTerm
    case x: OrSetTermKind =>
       x.orSet(term).mapReduce(x => this.resolve(x))(_ or _)(EmptyTerm)
    case x: AndSetTermKind =>
       x.andSet(term).mapReduce(x => this.resolve(x))(_ and _)(StarTerm.U)
    case x: OrElseTermKind =>
      x.cast(term).firstNotEmpty(_.resolve(term))
    case x: IfTermKind =>
      val ifx = x.guarded(term)
      // TODO: mb resolve ifx condition, think
      val check = KernelLanguage.evalCheck(ifx.condition,ifx.value)
      check match {
        case BooleanTerm(v) => if (v) resolve(ifx.value) else EmptyTerm
        case _ => IfTerm(resolve(ifx.value),check)
      }

  }


}



