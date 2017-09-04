package termware

trait AtomTerm extends PointTerm
{
  def arity = 0

  override def pointKind: PointKind = PointKind.Atom(this)
}

trait AtomTermImpl[S <: AtomTermImpl[S]] extends AtomTerm with PointTermImpl[S]
{
  this: S =>

}

case class DefaultAtomTerm(val name:Name) extends AtomTermImpl[DefaultAtomTerm] with EmptyContext
{
  override def pointKind: PointKind = PointKind.Atom(this)

  override def updateContext(ctx: MultiTerm): MultiTerm =
                                     ContextAtomTerm.create(this,ctx)

  override def uncontext = this

  override def mergeAsLeftContext(other: MultiTerm): MultiTerm = ???

  override def selectAsContextPattern(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeAnd(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeOr(other: MultiTerm): MultiTerm = ???

  override def selectAsLeftPattern(other: MultiTerm): MultiTerm = ???
}

case class ContextAtomTerm(origin:AtomTerm with EmptyContext, override val context: MultiTerm) extends ContextPointTerm(origin,context) with AtomTermImpl[ContextAtomTerm]
{
  override def arity = 0

  override def pointMergeAsLeftContext(other: PointTerm): MultiTerm = ???

  override def uncontext: AtomTerm with EmptyContext = origin
}

object ContextAtomTerm
{
  def create(a:AtomTerm,ctx:MultiTerm): MultiTerm =
    ctx.multiKind match {
      case MultiKind.Empty(e) => a
      case MultiKind.Error(e) => e
      case _ => ContextAtomTerm.create(a,ctx)
    }
}