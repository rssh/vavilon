package termware.impl

import termware._


class AndSetTermInExternalContext(term: AndSetTerm with NoExternalContext, externContext: MultiTerm)
  extends TermInExternalContext(term,externContext) with AndSetTerm
{

  override def mapReduce[A](map: MultiTerm => A)(reduce: (A, A) => A)(zero: => A): A = {
    term.mapReduce(map)(reduce)(zero)
  }

  override def members(): Seq[MultiTerm] = {
    term.members().map(_.addExternalContext(externContext))
  }

  override def unify(x: MultiTerm): MultiTerm = {
    // TODO: move up
    ifExternalContext(x.externalContext()){ joined =>
      TermInExternalContext(term unify x.dropExternalContext(),joined)
    }
  }

  override def or(x: MultiTerm): MultiTerm = {
    val join = externalContext and x.externalContext()
    if (join.isEmpty()) {
      OrSetTerm._fromSeq(Seq(this,x))
    } else if (join == externalContext && join == x.externalContext()){
      TermInExternalContext(term or x,join)
    } else {
      OrElseTerm(TermInExternalContext(term or x,join),OrSetTerm._fromSeq(Seq(this,x)))
    }
  }

  override def dropExternalContext(): AndSetTerm with NoExternalContext = term

}

object AndSetTermInExternalContext
{

  def apply(term: AndSetTerm with NoExternalContext, externContext: MultiTerm): AndSetTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      term
    } else {
      new AndSetTermInExternalContext(term, externContext)
    }
  }

}