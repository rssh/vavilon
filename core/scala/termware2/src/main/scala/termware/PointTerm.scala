package termware

trait PointTerm extends MultiTerm {

  def name: Name

  def arity: Int

  override def kind: PointTermKind

}


