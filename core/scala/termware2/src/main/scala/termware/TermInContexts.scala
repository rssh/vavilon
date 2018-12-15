package termware


abstract class TermInInternalContextOnly(term: NoExternalContext, val internContext: MultiTerm) extends MultiTerm with NoExternalContext
{


  //override def kind: MultiTermKind = term.kind


}

abstract class TermInExternalContext(term: NoExternalContext, externContext: MultiTerm) extends MultiTerm
{

  override def apply(term: PointTerm): MultiTerm = {
     this.term.apply(term)
  }

  override def and(x: MultiTerm): MultiTerm = {
    val joinContext = externalContext and x.externalContext
    if (joinContext.isEmpty()) {
      EmptyTerm
    } else {
      // TODO: rethink
      TermInExternalContext(term and x, joinContext)
    }
  }


  def defaultOrInExternalContext(x: MultiTerm): MultiTerm = {
    val joinContext = externContext and x.externalContext()
    if (joinContext == externContext) {
      TermInExternalContext(term or x, externContext)
    } else {
      OrSetTerm._fromSeq(Seq(this,x))
    }
  }

  override def resolve(term: MultiTerm): MultiTerm = term.subst(externalContext).resolve(term)

  override def externalContext: MultiTerm = externalContext

  override def dropExternalContext(): MultiTerm with NoExternalContext = term

  override def setExternalContext(context: MultiTerm): MultiTerm =
    TermInExternalContext(term, context)

  override def subst(context: MultiTerm): MultiTerm =
    TermInExternalContext(term.subst(context), externContext.subst(context))

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    term.pushInternalContext(context)
  }

}



object TermInExternalContext
{

  def apply(term: MultiTerm, externContext: MultiTerm): MultiTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      term
    } else if (!term.externalContext().isStar()) {
      TermInExternalContext(term.dropExternalContext(), term.externalContext() and externContext)
    } else {
      term.kind match {
        case k: EmptyTermKind => EmptyTerm
        case k: StarTermKind => ContextStarTerm(EmptyTerm,externContext)
        case k: PointTermKind => PointTermInExternalContext.create(k, k.pointTerm(term), externContext)
        case k: OrSetTermKind => val orSet = k.orSet(term)
            OrSetInExternalContext(orSet.dropExternalContext(),orSet.externalContext() and externContext)
        case k: AndSetTermKind => val andSet = k.andSet(term)
            AndSetTermInExternalContext(andSet.dropExternalContext(), andSet.externalContext() and externContext)
        case k: OrElseTermKind => val orElse = k.orElse(term)
            OrElseTermInExternalContext(orElse.dropExternalContext(), orElse.externalContext() and externContext)
      }
    }
  }

}

object PointTermInExternalContext
{

  def create(pk:PointTermKind,term: PointTerm, externalContext: MultiTerm): PointTerm = {
    pk match {
      case k: PrimitiveTermKind =>
        val primitiveTerm = k.primitive(term)
        new ContextfullPrimitiveTerm(primitiveTerm.base,primitiveTerm.context(),primitiveTerm.externalContext() and externalContext)
    }
  }

}

abstract class TermInContexts(term: NoExternalContext, internContext: MultiTerm, externContext: MultiTerm) extends TermInExternalContext(term, externContext)  {


  override def subst(context: MultiTerm): MultiTerm =
    TermInContexts(term.subst(context),internContext, this.externalContext.subst(context))

  override def and(x: MultiTerm): MultiTerm = {
    val joinContext = externalContext and x.externalContext
    if (joinContext.isEmpty()) {
      EmptyTerm
    } else {
      // TODO: rethink
      TermInContexts(term and x, internContext, joinContext)
    }
  }

  override def setExternalContext(context: MultiTerm): MultiTerm =
    TermInContexts(term, internContext, context)

  def pushContext(): MultiTerm = {
    if (externContext.isStar()) {
      this
    } else {
      TermInContexts(term, externToIntern(externContext) orElse internContext,externContext)
    }
  }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    TermInContexts(term, context orElse internContext, externContext)
  }

  def externToIntern(value: MultiTerm) : MultiTerm = {
    // TODO: andSet to OrSet
    value.kind match {
      case k: StarTermKind =>
        EmptyTerm
      case k: EmptyTermKind =>
        StarTerm.U   //  TODO:  external cont
      case k: PointTermKind => value
      case k: AndSetTermKind =>
        val andSet = k.andSet(value)
        andSet.mapReduce(identity[MultiTerm])(_ or _)(EmptyTerm)
      case k: OrSetTermKind =>
        k.orSet(value).mapReduce(identity[MultiTerm])(_ and _)(StarTerm.U)
      case k: OrElseTermKind =>
        k.orElse(value).map(externToIntern)
      case k: IfTermKind =>
        val guarded = k.guarded(value)
        IfTerm(externToIntern(guarded.value),guarded.condition)
    }
  }

}



object TermInContexts
{

  def apply(term: MultiTerm, internContext:MultiTerm, externContext: MultiTerm): MultiTerm = {
    term.kind match {
      case k: EmptyTermKind => EmptyTerm
      case k: StarTermKind => ContextStarTerm(internContext, externContext )
    }
  }

}


