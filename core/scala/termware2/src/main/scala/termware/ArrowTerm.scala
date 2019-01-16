package termware

import termware.util.FastRefOption

trait ArrowTerm extends PointTerm
{

  def left: MultiTerm

  def right: MultiTerm

  override def name: Name = KernelNames.arrowName

  override def arity: Int = 2

  override def kind: PointTermKind = ArrowTerm.Kind

}



object ArrowTerm  {

  def apply(left: MultiTerm, right: MultiTerm): MultiTerm = {
    // TODO: refine.
    impl.PlainArrowTerm(left,right)
  }

  object Kind extends ArrowTermKind {
    override def arrow(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
    override def cast(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
  }

}

object IsArrowTerm
{

  def unapply(arg: MultiTerm): FastRefOption[ArrowTerm] = {
    arg.kind match {
      case kt:ArrowTermKind => new FastRefOption[ArrowTerm](kt.arrow(kt.pointTerm(arg)))
      case _ => FastRefOption.empty
    }
  }

}