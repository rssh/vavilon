package termware

trait PointTerm extends MultiTerm {

  def arity: Int

  def pointKind: PointKind

  override def uncontext: PointTerm with EmptyContext

  override def cardinality: Int = 0

  override def multiKind: MultiKind = MultiKind.Point(this)


}


trait PointTermImpl[S <: PointTermImpl[S]] extends PointTerm with MultiTermImpl[S] {

  this : S =>

}


