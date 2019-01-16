package termware

trait IfTerm extends MultiTerm
{

  def value: MultiTerm

  def condition: PointTerm


  /**
    * Kind of term.
    * Used for marching by cases of kind, when we want form compiler exhausive case analysis)
    * (this is workaround for restricting case analysis to sealed traits in scala)
    *
    * @return kind of term (sealed case class)
    */
  override def kind: MultiTermKind = IfTermKind


  override def termApply(argument: PointTerm): MultiTerm = {
    val check = KernelLanguage.evalCheck(condition,value)
    check match {
      case BooleanTerm(v) =>
        if (v) {
          value.termApply(argument)
        } else {
          EmptyTerm
        }
      case other =>
        // guard failed.
        EmptyTerm
    }
  }

  /**
    * Unify
    *
    * @return
    */
  override def unify(x: MultiTerm): MultiTerm = {
    ifExternalContext(x.externalContext()){ joinContext =>
      // TODO: mb use joinContext ?
      val check = KernelLanguage.evalCheck(condition,value)
      check match {
        case BooleanTerm(v) =>
          if (v) {
            value.setExternalContext(joinContext) unify x.dropExternalContext()
          } else {
            EmptyTerm
          }
        case _ =>
          val nv = value.setExternalContext(joinContext) unify x.dropExternalContext()
          IfTerm(nv, check)
      }
    }
  }

  override def dropExternalContext(): IfTerm with NoExternalContext


}


case class PlainIfTerm(val value: MultiTerm, val condition:PointTerm) extends IfTerm with NoExternalContext {



  /**
    * resolve term in context of this
    *
    * @param term
    * @return
    */
  override def resolve(term: MultiTerm): MultiTerm =
    value.resolve(term)


  /**
    * Substitute term in context
    *
    * @param context in which we substitute one.
    * @return
    */
  override def subst(context: MultiTerm): MultiTerm = {
    val resolved = value.subst(context)
    val check = KernelLanguage.evalCheck(condition,resolved)
    check match {
      case BooleanTerm(v) =>
        if (v) resolved else EmptyTerm
      case _ =>
        IfTerm(resolved,check)
    }
  }



  override def or(x: MultiTerm): MultiTerm = {
    x.kind match {
      case k: EmptyTermKind => this
      case k: StarTermKind => x
      case k: OrSetTermKind => k.orSet(x) or this
      case k: AndSetTermKind => OrSetTerm._fromSeq(Seq(this,x))
      case k: OrElseTermKind => OrSetTerm._fromSeq(Seq(this,x))
      case k: PointTermKind => OrSetTerm._fromSeq(Seq(this,x))
      case k: IfTermKind => val gx = k.guarded(x)
        if (gx.value == value) {
          IfTerm(value,KernelLanguage.Or(condition,gx.condition))
        } else {
          OrSetTerm._fromSeq(Seq(this,x))
        }
    }
  }

  override def pushInternalContext(context: MultiTerm): PlainIfTerm = {
    new PlainIfTerm(value.pushInternalContext(context),condition)
  }

  override def dropExternalContext(): PlainIfTerm = this

}

class IfTermInExternalContext(term: PlainIfTerm, externContext: MultiTerm)
   extends TermInExternalContext(term,externContext)
   with IfTerm
{

  override def value: MultiTerm = term.value

  override def condition: PointTerm = term.condition

  override def pushInternalContext(context: MultiTerm): IfTerm = {
    new IfTermInExternalContext(term.pushInternalContext(context),externContext)
  }

  override def or(x: MultiTerm): MultiTerm = defaultOrInExternalContext(x)

  override def dropExternalContext(): IfTerm with NoExternalContext = term

}

object IfTermInExternalContext
{
  def apply(term: IfTerm with NoExternalContext, externContext: MultiTerm): MultiTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      term
    } else if (term.isInstanceOf[PlainIfTerm]) {
      new IfTermInExternalContext(term.asInstanceOf[PlainIfTerm], externContext)
    } else {
      new IfTermInExternalContext(PlainIfTerm(term.value,term.condition), externContext)
    }
  }
}


object IfTerm
{

  object Kind extends IfTermKind

  def apply(value: MultiTerm, condition: PointTerm): MultiTerm = {
    if (value.externalContext().isStar() && condition.isStar()) {
      value.kind match {
        case k: EmptyTermKind => EmptyTerm
        case k: IfTermKind => val iv = k.guarded(value)
          IfTerm(iv.value,KernelLanguage.And(condition,iv.condition))
        case k: StarTermKind =>
          new PlainIfTerm(value,condition)
        case k: OrSetTermKind =>
          k.orSet(value).mapReduce(IfTerm(_,condition))(_ or _)(EmptyTerm)
        case _: AndSetTermKind | _:PointTermKind | _:OrElseTermKind =>
          new PlainIfTerm(value,condition)
      }
    } else {
      value.ifExternalContext(condition.externalContext()) { joinContext =>
        val pointCondition = condition.dropExternalContext().asInstanceOf[PointTerm]
        val wec = IfTerm(value.dropExternalContext(),condition.dropExternalContext())
        wec.kind match {
          case k: EmptyTermKind => wec
          case k: IfTermKind =>
            if (wec.externalContext().isStar()) {
              val g = k.guarded(wec)
              if (g.isInstanceOf[PlainIfTerm]) {
                new IfTermInExternalContext(g.asInstanceOf[PlainIfTerm],joinContext)
              } else {
                new IfTermInExternalContext(PlainIfTerm(g.value, g.condition), joinContext)
              }
            } else {
              // impossible
              TermInExternalContext(wec,joinContext)
            }
          case _ => TermInExternalContext(wec,joinContext)
        }
      }
    }
  }


}