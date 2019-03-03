package termware

import termware.util.FastRefOption

trait ArrowTermOps extends PointTermOps
{

  this:ArrowTerm =>

  def left: MultiTerm

  def right: MultiTerm

  override def name: Name = KernelNames.arrowName

  override def arity: Int = 2

  override def kind: PointTermKind = ArrowTerm.Kind

}



object ArrowTermOps  {

  def apply(left: MultiTerm, right: MultiTerm): MultiTerm = {
    // TODO: refine.
    impl.PlainArrowTerm(left,right)
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