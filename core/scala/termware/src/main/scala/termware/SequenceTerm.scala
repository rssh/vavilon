package termware


trait SequenceTerm extends PointTerm {

  def subterm(i:Int): MultiTerm

  def subterms(): IndexedSeq[MultiTerm]

  def append(x:MultiTerm): SequenceTerm

  override def pointKind: PointKind = PointKind.Sequence(this)

  override def uncontext: SequenceTerm with EmptyContext

  override def name: Name = SeqName

  override def pointUnify(other: PointTerm): MultiTerm =
     other.pointKind match {
       case PointKind.Sequence(otherSeq) =>
         if (otherSeq.arity != arity) {
           EmptyTerm
         } else {
           val nsubs = subterms().zip(otherSeq.subterms()).map( x => x._1 unify x._2  )
           if (nsubs.forall(_.isExist)) {
             val plain = new PlainSequenceTerm(nsubs)
             ContextSequenceTerm.create(plain,context)
           } else {
             ContextContradictionTerm.withMessages("operation" -> "unification")
           }
         }
       case _ => EmptyTerm
     }

  def newSubterms(value:IndexedSeq[MultiTerm]): PointTerm

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

  override def in(ctx: MultiTerm): MultiTerm = ContextSequenceTerm(this,ctx)

  override def orPoint(other: PointTerm): MultiTerm =
     other.pointKind match {
       case PointKind.Sequence(otherSeq) =>
          if (termEq(otherSeq)) {
            this
          } else {
            SetTerm.create(this,otherSeq)
          }
       case _ => SetTerm.create(this,other)
     }

  override def andPoint(other: PointTerm): MultiTerm =
     other.pointKind match {
       case PointKind.Sequence(otherSeq) =>
         if (this.arity == otherSeq.arity) {
           val v = (values zip otherSeq.subterms()).map{
             case (x,y) => x and y
           }
           v.find(_.isEmpty) match {
             case Some(_) => EmptyTerm
             case None => new PlainSequenceTerm(v)
           }
         }else{
           EmptyTerm
         }
       case _ => EmptyTerm
     }

  override def eval(other: MultiTerm): MultiTerm = ???

  override def subst(x: MultiTerm): MultiTerm =
  {
    new PlainSequenceTerm(values.map(_.subst(x)))
  }

  override def append(x: MultiTerm): SequenceTerm =
  {
    new PlainSequenceTerm(values :+ x)
  }
}

object PlainSequenceTerm
{

  def create(values: MultiTerm*): PlainSequenceTerm =
  {
    new PlainSequenceTerm(values.toIndexedSeq)
  }

  val empty = new PlainSequenceTerm(IndexedSeq())

}

case class ContextSequenceTerm(origin: SequenceTerm with EmptyContext, override val context: MultiTerm) extends ContextPointTerm(origin, context) with SequenceTermImpl[ContextSequenceTerm]
{
  override def subterm(i: Int): MultiTerm =
    ContextMultiTerm.create(origin.subterm(i),context)

  override def subterms(): IndexedSeq[MultiTerm] =
    origin.subterms().map(ContextMultiTerm.create(_,context))

  override def orPoint(other: PointTerm): MultiTerm = ???

  override def andPoint(other: PointTerm): MultiTerm = ???

  /**
    * narrow x to be compatible with this as context
    */
  override def narrow(x: PointTerm): PointTerm = ???

  override def eval(other: MultiTerm): MultiTerm = ???

  override def updateContext(ctx: MultiTerm): Unit = ???

  override def uncontext: SequenceTerm with EmptyContext = origin


  override def append(x: MultiTerm): SequenceTerm = ???


}


object ContextSequenceTerm
{
  def create(origin: SequenceTerm, ctx:MultiTerm):MultiTerm =
    ctx.multiKind match {
      case MultiKind.Empty(e) => origin
      case MultiKind.Contradiction(e) => e
      case _ => if (origin.context.isEmpty) {
        ContextSequenceTerm(origin.uncontext,ctx)
      } else {
        origin.in(ctx)
      }
    }
}
