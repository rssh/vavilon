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


  override def apply(argument: PointTerm): MultiTerm = {
    val check = KernelLanguage.evalCheck(condition,value,EmptyTerm)
    check match {
      case BooleanTerm(v) =>
        if (v) {
          value.apply(argument)
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
      val check = KernelLanguage.evalCheck(condition,value,joinContext)
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
    val check = KernelLanguage.evalCheck(condition,value,context)
    check match {
      case BooleanTerm(v) =>
        if (v) resolved else EmptyTerm
      case _ =>
        IfTerm(value,check)
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

}

class IfTermInExternalContext(term: PlainIfTerm, externContext: MultiTerm)
   extends TermInExternalContext(term,externContext)
   with IfTerm
{

  override def value: MultiTerm = term.value

  override def condition: PointTerm = term.condition

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    new IfTermInExternalContext(term.pushInternalContext(context),externContext)
  }

  override def or(x: MultiTerm): MultiTerm = defaultOrInExternalContext(x)

}

object IfTerm
{

  object Kind extends IfTermKind

  def apply(value: MultiTerm, condition: PointTerm): MultiTerm = {
    value.kind match {
      case k: EmptyTermKind => EmptyTerm
      case k: IfTermKind => val iv = k.guarded(value)
        IfTerm(iv.value,KernelLanguage.And(condition,iv.condition))
    }
  }


}