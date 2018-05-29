package termware

import termware.util.FastRefOption

trait AtomTerm extends PointTerm
{
  override def arity: Int = 0

}

class ContextAtomTerm(override val name: AtomName, context:MultiTerm = EmptyTerm) extends AtomTerm {

  override def kind = ContextAtomTerm

}

object ContextAtomTerm extends AtomTermKind
{
  override def atomTerm(x: PointTerm): AtomTerm = x.asInstanceOf[AtomTerm]
}

object AtomTerm
{
  def apply(sname:String) = AtomName(sname)

  def unapply(arg: AtomTerm): FastRefOption[AtomTerm] = {
     new FastRefOption(arg)
  }
}
