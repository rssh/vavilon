package termware

import termware.algo.ContextMerge
import termware.util.FastRefOption

trait AtomTerm extends PointTerm with ContextCarrierTerm
{

  override def name: AtomName

  override def arity: Int = 0

  def context(): MultiTerm

  override def pointUnify(ptk: PointTermKind, u: InContext[PointTerm]): TermInContext = {
    u.term.kind match {
      case kind: AtomTermKind =>
        if (u.term.name == name) {
          u
        } else {
          TermInContext.empty
        }
      case _ => TermInContext.empty
    }
  }

  override def subst(context: MultiTerm): MultiTerm = {
    val to = context.resolve(this)
    if (to.isEmpty()) this else to
  }


}

class ContextAtomTerm(override val name: AtomName, override val context:MultiTerm = EmptyTerm) extends AtomTerm {

  override def kind = ContextAtomTerm

}

object ContextAtomTerm extends AtomTermKind
{

  def apply(name:AtomName, context:MultiTerm = EmptyTerm): AtomTerm = {
    if (context.isEmpty()) {
      name
    } else {
      ContextAtomTerm(name,context)
    }
  }

  override def atomTerm(x: PointTerm): AtomTerm = x.asInstanceOf[AtomTerm]
}

object AtomTerm
{
  def apply(sname:String) = AtomName(sname)

  def unapply(arg: AtomTerm): FastRefOption[AtomTerm] = {
     new FastRefOption(arg)
  }
}
