package termware


trait SequenceTerm extends PointTerm {

  def subterm(i:Int): MultiTerm

  def subterms(): IndexedSeq[MultiTerm]

  override def pointKind: PointKind = PointKind.Sequence(this)

  override def uncontext: SequenceTerm with EmptyContext

  override def name: Name = SeqName

}

trait SequenceTermImpl[S<:SequenceTermImpl[S]] extends SequenceTerm with PointTermImpl[S]
{
  this: S =>

}

case class DefaultSequenceTerm(values:IndexedSeq[MultiTerm]) extends SequenceTermImpl[DefaultSequenceTerm]
{
  override def subterm(i: Int): MultiTerm = ???

  override def subterms(): IndexedSeq[MultiTerm] = ???

  override def arity: Int = ???

  override def context: MultiTerm = ???

}


case class ContextSequenceTerm(origin: SequenceTerm with EmptyContext, override val context: MultiTerm) extends ContextPointTerm(origin, context) with SequenceTermImpl[ContextSequenceTerm]
{
  override def subterm(i: Int): MultiTerm =
    ContextMultiTerm.create(origin.subterm(i),context)

  override def subterms(): IndexedSeq[MultiTerm] =
    origin.subterms().map(ContextMultiTerm.create(_,context))


}


object ContextSequenceTerm
{
  def create(origin: SequenceTerm, ctx:MultiTerm):MultiTerm =
    ctx.multiKind match {
      case MultiKind.Empty(e) => origin
      case MultiKind.Error(e) => e
      case _ => if (origin.context.isEmpty) {
        ContextSequenceTerm(origin.uncontext,ctx)
      } else {
        origin.updateContext(ctx)
      }
    }
}
