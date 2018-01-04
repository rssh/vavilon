package termware

/**
  * Sequence of arrows (or set of arrows), each of them are more general, that next term.
  */
trait SeqOrTerm extends MultiTerm {


  def head:MultiTerm

  def tail:MultiTerm

  // laws: head less tail

  override def uncontext: SeqOrTerm with EmptyContext

  override def unify(x: MultiTerm): MultiTerm =
    (head unify x).multiKind match {
      case MultiKind.Contradiction(c) => tail unify x
      case MultiKind.Empty(e) => tail unify x
      case other => other.x
    }


  def map(f:MultiTerm => MultiTerm):MultiTerm

  override def subst(x: MultiTerm): MultiTerm =
    map (_ subst x)

  override def check(x: PointTerm): Boolean =
    find( _.check(x) ).multiKind match {
      case MultiKind.Empty(e) => false
      case MultiKind.Contradiction(ct) => false
      case _ => true
    }

  def find(p: MultiTerm => Boolean): MultiTerm =
  {
    if (p(head)) {
       head
    } else {
      tail.multiKind match {
        case MultiKind.Empty(e) => e
        case MultiKind.SeqOr(s) => s.find(p)
        case MultiKind.Set(s) => s.find(p)
        case MultiKind.Contradiction(ct) => ct
        case MultiKind.Star(s) => if (p(s)) s else EmptyTerm
        case MultiKind.Point(pt) => if (p(pt)) pt else EmptyTerm
      }
    }
  }

}



trait SeqOrTermImpl[This<:SeqOrTermImpl[This]] extends SeqOrTerm
{


}


case class DefaultSeqOrTerm(override val head:MultiTerm, override val tail:MultiTerm) extends SeqOrTermImpl[DefaultSeqOrTerm] with EmptyContext
{

  override def name: Name = OrElseName

  override def cardinality: Int = 1 + tail.cardinality

  override def uncontext: SeqOrTerm with EmptyContext = this

  override def multiKind: MultiKind = MultiKind.SeqOr(this)

  override def in(ctx: MultiTerm): MultiTerm = map (_ in ctx)

  override def inside(ctx: MultiTerm): MultiTerm = map( _ inside ctx)

  override def or(other: MultiTerm): MultiTerm = ???

  override def and(other: MultiTerm): MultiTerm = ???

  override def apply(x: MultiTerm): MultiTerm =
  {
    head.apply(x).multiKind match {
      case MultiKind.Empty(e) => tail.apply(x)
      case other => other.x
    }
  }


  def map(f:MultiTerm => MultiTerm):MultiTerm = ???

}

object SeqOrTerm
{

  def create(values: MultiTerm*):MultiTerm =
  {
    fromList(values.toList)
  }

  def fromList(values:List[MultiTerm]):MultiTerm =
    values match {
      case Nil => EmptyTerm
      case h::t => if (t.isEmpty) h
                    else if (h.isEmpty) fromList(t)
                    else new DefaultSeqOrTerm(h,fromList(t))
    }

}