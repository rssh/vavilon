package object termware {

  val ContradictionTermKind = ContradictionTerm

  final case class InContext[+T <: MultiTerm](term: T, context: MultiTerm)
  {

    def and(other: InContext[MultiTerm]): InContext[MultiTerm] = {
      val nContext = context.and(other.context).resolved()
      if (nContext.isContradiction()) {
        InContext(EmptyTerm,nContext)
      } else {
         InContext(term.and(other.term),nContext)
      }
    }

    @inline
    def or(other: InContext[MultiTerm]): InContext[MultiTerm] = {
      TermInContext(term.or(other.term),context or other.context)
    }


  }

  type TermInContext = InContext[MultiTerm]

  type SetTermInContext = InContext[SetTerm]

  type PointTermInContext = InContext[PointTerm]

  object TermInContext
  {
    @inline def apply(t:MultiTerm,c:MultiTerm): TermInContext = InContext[MultiTerm](t,c)
    val empty = EmptyTerm^^EmptyTerm
  }

}
