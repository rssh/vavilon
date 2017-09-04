package termware

trait StructuredTerm extends PointTerm
{

  def get(n:Name): MultiTerm

  def subterms():IndexedSeq[MultiTerm]

  override def pointKind: PointKind = PointKind.Structured(this)

  override def uncontext: StructuredTerm with EmptyContext

}

trait StructuredTermImpl[S <: StructuredTermImpl[S]] extends StructuredTerm with PointTermImpl[S] {

  this: S =>


}

case class DefaultStructuredTerm(indexes:NameIndexed[MultiTerm])


