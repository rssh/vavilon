package termware

trait SetTerm extends MultiTerm {

  override def name: Name = SetName

  override def multiKind: MultiKind = MultiKind.Set(this)

  def find(p: PointTerm => Boolean): MultiTerm

  def mapOr(f: PointTerm => MultiTerm): MultiTerm =
       mapReduce[MultiTerm](EmptyTerm)(f)(_ or _)

  def mapAnd(f: PointTerm => MultiTerm): MultiTerm =
        mapReduce[MultiTerm](DefaultStarTerm)(f)(_ and _)

  def mapReduce[B](s0:B)(f:PointTerm=>B)(r:(B, B)=>B):B

  //def mapReduceIf[B](f:PointTerm=>B)(p:B=>Boolean)(r:(B, B)=>B):MultiTerm


}


case class DefaultSetTerm(data:Seq[PointTerm]) extends SetTerm with EmptyContext
{

  override type Self = DefaultSetTerm

  override def cardinality: Int = data.size

  override def in(ctx: MultiTerm): MultiTerm = mapOr( _ in ctx)

  override def inside(ctx: MultiTerm): MultiTerm = mapOr(_ inside ctx)

  override def check(x:PointTerm): Boolean =
    data.find(_.check(x)).nonEmpty

  override def or(other: MultiTerm): MultiTerm = ???

  override def and(other: MultiTerm): MultiTerm = ???

  override def apply(other: MultiTerm): MultiTerm =
    mapOr(_ apply other)

  override def unify(x: MultiTerm): MultiTerm =
    x.multiKind match {
      case MultiKind.Empty(e) => e
      case MultiKind.Contradiction(c) => c
      case other => mapOr(_ unify other.x)
    }

  override def subst(x: MultiTerm): MultiTerm =
    mapOr(_ subst x)

  override def find(p: (PointTerm) => Boolean): MultiTerm =
   data.find(p).getOrElse(EmptyTerm)

  override def mapReduce[B](s0:B)(f: (PointTerm) => B)(r: (B, B) => B): B =
  {
    data.map(f).fold(s0)(r)
  }

}

object SetTerm
{


  def create(values:MultiTerm*): MultiTerm =
  {
    if (values.isEmpty) {
      EmptyTerm
    } else if (values.tail.isEmpty) {
      values.head
    } else {
      values.fold(EmptyTerm){ (s,e) => s or e }
    }
  }


  def createPoints(values: PointTerm*): MultiTerm =
  {
    if (values.isEmpty) {
      EmptyTerm
    } else {
      DefaultSetTerm(values)
    }
  }

}
