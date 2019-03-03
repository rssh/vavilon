package termware

import termware.util.{FastRefOption, NameIndexed}

trait AtomTermOps extends PointTermOps with ContextCarrierTerm
                          with StringLikeName
{

  this: AtomTerm =>

  override def kind: PointTermKind = AtomTerm.Kind

  override def name: ContextLessAtomTerm

  override def arity: Int = 0

  def context(): MultiTerm

  override def pointUnify(ptk: PointTermKind, u: PointTerm): MultiTerm = {
    u.kind match {
      case k:AtomTermKind =>
        val otherAtom = k.atomTerm(u)
        if (name == otherAtom.name) {
          u
        } else {
          EmptyTerm
        }
      case _ => EmptyTerm
    }
  }

  override def subst(context: MultiTerm): MultiTerm = {
    val to = context.resolve(this)
    if (to.isEmpty()) this else to
  }

  // DSL
  def apply(values: (AtomTerm,MultiTerm)* ): StructuredTerm = {
    StructuredTerm.create(this, NameIndexed.fromSeq(values))
  }

  // Name:
  override def typeIndex(): Int = {
    TypeIndexes.ATOM
  }

}


final case class ContextLessAtomTerm(override val value: String) extends AtomTerm with PointTermNoExternalContext {

  override def name: ContextLessAtomTerm = this

  override def context(): MultiTerm = EmptyTerm

  override def setExternalContext(context: MultiTerm): MultiTerm =
    if (context.isStar()) {
      this
    } else {
      ContextfullAtomTerm(this,EmptyTerm,context)
    }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    if (context.isEmpty()) {
      this
    } else {
      new AtomTermInInternalContext(this,context)
    }
  }

}

object AtomName
{

  @inline final def apply(value:String) = ContextLessAtomTerm(value)

}

case class AtomTermInInternalContext(
    override val name: ContextLessAtomTerm,
    override val context:MultiTerm) extends
   TermInInternalContextOnly(name,context) with AtomTerm with PointTermNoExternalContext
{
  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    new AtomTermInInternalContext(name, context orElse this.context )
  }

  override def value: String = name.value


}

case class ContextfullAtomTerm(override val name: ContextLessAtomTerm,
                           override val context:MultiTerm = EmptyTerm,
                           externContext: MultiTerm = StarTerm.U
                          ) extends TermInContexts(name,context,externContext)
                              with AtomTerm  {

  override def dropExternalContext(): PointTerm with NoExternalContext = {
    if (context.isEmpty()) {
      name
    } else {
      new AtomTermInInternalContext(name, context)
    }
  }

  override def value: String = name.value

}


object ContextfullAtomTerm
{

  def apply(name: ContextLessAtomTerm, context:MultiTerm, externContext: MultiTerm): AtomTerm = {
    if (externContext.isStar()) {
      if (context.isEmpty()) {
        name
      } else {
        new ContextfullAtomTerm(name, context)
      }
    } else {
      new ContextfullAtomTerm(name,context,externContext)
    }
  }


}

