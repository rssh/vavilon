package termware

case class ArrowTerm(left: MultiTerm, right: MultiTerm) extends PointTerm
{
  override def name: Name = KernelNames.arrowName

  override def arity: Int = 2

  override def kind: PointTermKind = ArrowTerm
}



object ArrowTerm extends ArrowTermKind {
  override def arrow(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
}
