package termware

trait SetTerm extends MultiTerm {

  override def name: Name = SetName

  override def multiKind: MultiKind = MultiKind.Set(this)

  def mapMerge(f:MultiTerm => MultiTerm): MultiTerm

}

trait SetTermImpl[S <: SetTermImpl[S]] extends SetTerm with MultiTermImpl[S]
{
  this: S =>


}

case class DefaultSetTerm(data:NameIndexed[PointTerm]) extends SetTermImpl[DefaultSetTerm] with EmptyContext
{

  override def mapMerge(f: (MultiTerm) => MultiTerm): MultiTerm = ???

  override def cardinality: Int = data.size

  override def multiKind: MultiKind = ???

  override def context: MultiTerm = ???

  override def updateContext(ctx: MultiTerm): MultiTerm = ???

  override def mergeAsLeftContext(other: MultiTerm): MultiTerm = ???

  override def selectAsContextPattern(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeAnd(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeOr(other: MultiTerm): MultiTerm = ???

  override def selectAsLeftPattern(other: MultiTerm): MultiTerm = ???
}


