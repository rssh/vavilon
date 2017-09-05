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

case class PlainSequenceTerm(values:IndexedSeq[MultiTerm]) extends SequenceTermImpl[PlainSequenceTerm] with EmptyContext
{
  override def subterm(i: Int): MultiTerm = {
    if (i < values.size && i>0) {
      values(i)
    }else{
      EmptyTerm
    }
  }

  override def subterms(): IndexedSeq[MultiTerm] = values

  override def arity: Int = values.size

  override def uncontext: SequenceTerm with EmptyContext = this

  override def updateContext(ctx: MultiTerm): MultiTerm = ???

  override def mergeAsLeftContext(other: MultiTerm): MultiTerm = ???

  override def selectAsContextPattern(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeAnd(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeOr(other: MultiTerm): MultiTerm = ???

  override def selectAsLeftPattern(other: MultiTerm): MultiTerm = ???
}


case class ContextSequenceTerm(origin: SequenceTerm with EmptyContext, override val context: MultiTerm) extends ContextPointTerm(origin, context) with SequenceTermImpl[ContextSequenceTerm]
{
  override def subterm(i: Int): MultiTerm =
    ContextMultiTerm.create(origin.subterm(i),context)

  override def subterms(): IndexedSeq[MultiTerm] =
    origin.subterms().map(ContextMultiTerm.create(_,context))

  override def pointMergeAsLeftContext(other: PointTerm): MultiTerm = ???

  override def uncontext: SequenceTerm with EmptyContext = origin
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
