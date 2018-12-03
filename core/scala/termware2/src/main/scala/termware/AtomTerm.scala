package termware

import termware.algo.ContextMerge
import termware.util.FastRefOption

trait AtomTerm extends PointTerm with ContextCarrierTerm
{

  override def kind: PointTermKind = AtomTerm.Kind

  override def name: AtomName

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


}


case class ContextLessAtomTerm(override val name: AtomName) extends AtomTerm with NoExternalContext {

  override def context(): MultiTerm = EmptyTerm

  override def setExternalContext(context: MultiTerm): MultiTerm =
    if (context.isStar()) {
      this
    } else {
      ContextAtomTerm(name,EmptyTerm,context)
    }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    if (context.isEmpty()) {
      this
    } else {
      ContextAtomTerm(name,context,StarTerm.U)
    }
  }

}


case class ContextAtomTerm(override val name: AtomName,
                           override val context:MultiTerm = EmptyTerm,
                           externContext: MultiTerm = StarTerm.U
                          ) extends TermInContexts(name,context,externContext) with AtomTerm {


}


object ContextAtomTerm
{

  def apply(name:AtomName, context:MultiTerm): AtomTerm = {
    if (context.isEmpty()) {
      name
    } else {
      ContextAtomTerm(name,context)
    }
  }


}

object AtomTerm
{

  object Kind extends AtomTermKind {
    override def atomTerm(x: PointTerm): AtomTerm = x.asInstanceOf[ContextLessAtomTerm]
  }

  def apply(sname:String) = AtomName(sname)

  def unapply(arg: AtomTerm): FastRefOption[AtomTerm] = {
     new FastRefOption(arg)
  }
}

