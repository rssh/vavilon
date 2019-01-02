package termware

trait StarTerm extends MultiTerm with AndSetTerm
{

  def context: MultiTerm

  override lazy val kind: MultiTermKind = StarTerm.Kind

  override def subst(context: MultiTerm): MultiTerm = this

  override def mapReduce[A](map: MultiTerm => A)(reduce: (A, A) => A)(zero: => A): A = {
    zero
  }

  override def members(): Seq[MultiTerm] = Seq.empty

}

object ContextLessStarTerm extends StarTerm with NoExternalContext
{
  override def context: MultiTerm = EmptyTerm

  override def resolve(term: MultiTerm): MultiTerm = term

  override def apply(term: PointTerm): MultiTerm = EmptyTerm

  override def unify(arg: MultiTerm): MultiTerm = arg

  override def or(x: MultiTerm): MultiTerm = this

  override def and(x: MultiTerm): MultiTerm = x

  override def orElse(x: MultiTerm): MultiTerm = this

  override def cond(x: PointTerm): MultiTerm = new PlainIfTerm(this, x)

  override def externalContext(): MultiTerm = this

  override def setExternalContext(context: MultiTerm): MultiTerm = {
     new ContextStarTerm(EmptyTerm,context)
  }

  override def dropExternalContext(): StarTerm with NoExternalContext = this

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    if (context.isEmpty()) {
      this
    } else {
      new ContextStarTerm(context, StarTerm.U)
    }
  }

}

object StarTerm {

  object Kind extends StarTermKind

  val U = ContextLessStarTerm


}


case class ContextStarTerm(internalContext: MultiTerm, override val externalContext: MultiTerm) extends TermInContexts(ContextLessStarTerm, internalContext, externalContext) with StarTerm {


  override def context: MultiTerm = internalContext

  override def apply(term: PointTerm): MultiTerm =
                                context.resolve(term)

  override def resolve(term: MultiTerm): MultiTerm =
                                EmptyTerm

  override def unify(u: MultiTerm): MultiTerm = {
    ifExternalContext(u.externalContext()) { joinContext =>
        TermInContexts(u,joinContext,EmptyTerm)
    }
  }

  override def and(x: MultiTerm): MultiTerm = {
    val u = unify(x)
    u.subst(u.externalContext()).dropExternalContext()
  }

  override def or(x: MultiTerm): MultiTerm = {
    if (externalContext.isStar() || this.externalContext == x.externalContext()) {
      if (internalContext.isEmpty())
        StarTerm.U
      else {
        this
      }
    } else if (x.externalContext.isStar()) {
        //  *(L), x(*)
        if (x.isStar()) {
          // *(L), *(*)
          x
        } else {
          OrSetTerm.create(x,this)
        }
    } else {
       // *(L1), x(L2)
        OrSetTerm.create(x, this)
    }
  }

  override def dropExternalContext(): StarTerm with NoExternalContext = StarTerm.U

}





