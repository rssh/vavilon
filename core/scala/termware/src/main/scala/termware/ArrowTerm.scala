package termware

trait ArrowTerm extends PointTerm
{
  def left: MultiTerm
  def right: MultiTerm

  override def pointKind: PointKind = PointKind.Arrow(this)


  override def uncontext: ArrowTerm with EmptyContext


}


trait ArrowTermImpl[S <: ArrowTermImpl[S]] extends ArrowTerm with PointTermImpl[S]
{
  this: S =>

}


case class DefaultArrowTerm(val left: MultiTerm, right: MultiTerm) extends ArrowTermImpl[DefaultArrowTerm] with EmptyContext
{
  override def name = ArrowName
  override def arity: Int = 0

  override def uncontext: ArrowTerm with EmptyContext = this
  override def updateContext(ctx: MultiTerm): MultiTerm = ContextArrowTerm.create(this,ctx)

  override def mergeAsLeftContext(other: MultiTerm): MultiTerm = ???

  override def selectAsContextPattern(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeAnd(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeOr(other: MultiTerm): MultiTerm = ???

  override def selectAsLeftPattern(other: MultiTerm): MultiTerm = ???
}

case class ContextArrowTerm(origin: ArrowTerm with EmptyContext, override val context:MultiTerm) extends ContextPointTerm(origin,context) with ArrowTermImpl[ContextArrowTerm]
{
  override def pointMergeAsLeftContext(other: PointTerm): MultiTerm = ???

  override def left: MultiTerm = ContextMultiTerm.create(origin.left,context)

  override def right: MultiTerm = ContextMultiTerm.create(origin.left,context)

  override def uncontext: ArrowTerm with EmptyContext = origin

}

object ContextArrowTerm
{

  def create(a: ArrowTerm,context:MultiTerm):MultiTerm =
    if (a.context.isEmpty) {
      context.multiKind match {
        case MultiKind.Empty(e) => a
        case MultiKind.Error(e) => e
        case MultiKind.Star(s) => create(a, s.context)
        case _ => ContextArrowTerm(a.uncontext, context)
      }
    } else {
      a.updateContext(context)
    }

}
