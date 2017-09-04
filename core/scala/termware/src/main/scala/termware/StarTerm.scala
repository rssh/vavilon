package termware


trait StarTerm extends MultiTerm
{
  override def name: Name = StarName

  override def cardinality: Int = Int.MaxValue

  override def multiKind: MultiKind = MultiKind.Star(this)


}


/**
  * Plain Star term without context.
  */
case object DefaultStarTerm extends StarTerm with EmptyContext {

  override def context: MultiTerm = EmptyTerm

  override def updateContext(ctx: MultiTerm): MultiTerm =
      ctx.multiKind match {
        case MultiKind.Star(x) => ???
        case MultiKind.Error(e) => e
        case MultiKind.Empty(e) => this
        case MultiKind.Set(e) => ContextStarTerm(e)
        case MultiKind.Point(p) => ContextStarTerm(p)
      }


}

case class ContextStarTerm(context:MultiTerm) extends  ContextMultiTerm(DefaultStarTerm,context) with StarTerm with MultiTermImpl[ContextStarTerm]
{
  override def updateContext(ctx: MultiTerm): MultiTerm =
    ctx.multiKind match {
      case MultiKind.Empty(e) => this
      case MultiKind.Error(e) => e
      case r => ContextStarTerm.create(ctx.mergeAsLeftContext(context))
    }

  override def uncontext: MultiTerm with EmptyContext = DefaultStarTerm

  override def mergeAsLeftContext(other: MultiTerm): MultiTerm = ???

  override def selectAsContextPattern(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeAnd(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeOr(other: MultiTerm): MultiTerm = ???

  override def selectAsLeftPattern(other: MultiTerm): MultiTerm = ???
}

object ContextStarTerm
{
  def create(ctx:MultiTerm):MultiTerm =
    ctx.multiKind match {
      case MultiKind.Empty(e) => DefaultStarTerm
      case MultiKind.Error(e) => e
      case _ => ContextStarTerm(ctx)
    }
}
