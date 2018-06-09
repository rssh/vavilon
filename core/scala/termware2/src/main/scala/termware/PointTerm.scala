package termware

trait PointTerm extends MultiTerm {

  def name: Name

  def arity: Int

  def context(): MultiTerm

  override def kind: PointTermKind

  override lazy val resolved: MultiTerm = context.resolve(this)

  override def resolve(term: MultiTerm): MultiTerm =
    term.kind match {
      case x: PointTermKind => apply(x.pointTerm(term))
      case x: EmptyTermKind => term
    }

  override def apply(term: PointTerm): MultiTerm = EmptyTerm

}

object PointTerm
{

}


