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

case class DefaultSetTerm(data:NameIndexed[PointTerm]) extends SetTermImpl[DefaultSetTerm]
{

  override def mapMerge(f: (MultiTerm) => MultiTerm): Unit = ???

  override def cardinality: Int = data.size

  override def multiKind: MultiKind = ???

  override def select(pattern: MultiTerm): MultiTerm = ???

  override def context: MultiTerm = ???

  override def contextOr(otherContext: MultiTerm): MultiTerm = ???

  override def contextAnd(otherContext: MultiTerm): MultiTerm = ???
}


