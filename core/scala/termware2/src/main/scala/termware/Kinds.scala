package termware


import termware.util.FastRefOption

sealed trait MultiTermKind
{
  type Out <: MultiTerm
  type In <: MultiTerm

  def cast(x:In):Out =
    x.asInstanceOf[Out]


}

sealed trait NonContradictionTermKind extends MultiTermKind

sealed trait PointTermKind extends NonContradictionTermKind
{
  type In <: MultiTerm
  type Out <: PointTerm

  @inline
  def pointTerm(x:MultiTerm): PointTerm = {
    x.asInstanceOf[PointTerm]
  }

  @inline
  def checkPointTerm(x:MultiTerm): FastRefOption[PointTerm] = {
    if (x.isInstanceOf[PointTerm]) {
      FastRefOption(x.asInstanceOf[PointTerm])
    } else {
      FastRefOption.empty
    }
  }


}



trait AtomTermKind extends PointTermKind
{
  override type In = PointTerm
  override type Out = AtomTerm

  def atomTerm(x:PointTerm):AtomTerm

}

trait PrimitiveTermKind extends PointTermKind
{

  override type In = PointTerm
  override type Out = PrimitiveTerm[PrimitiveTerm[_]#Value]

  type Primitive

  def primitive(x:PointTerm): Out

  def tprimitive[T](x:PointTerm): PrimitiveTerm[T] =
    primitive(x).asInstanceOf[PrimitiveTerm[T]]

}



trait SingletonNameKind extends PointTermKind
{
  override type In = PointTerm
  override type Out = SingletonName

  def singleton(in: PointTerm):Out = cast(in)

  def baseSingletonName(): BaseSingletonName

}

trait ArrowTermKind extends PointTermKind
{
  type In = PointTerm
  type Out = ArrowTerm
  def arrow(x:PointTerm): ArrowTerm

}

trait StructuredTermKind extends PointTermKind
{
  type In = PointTerm
  type Out = StructuredTerm

  @inline
  def structured(x:PointTerm): StructuredTerm = cast(x)

}

object StructuredTermKind extends StructuredTermKind
{


}


trait EmptyTermKind extends NonContradictionTermKind
{
  type In = MultiTerm
  type Out = EmptyTerm.type
}

object EmptyTermKind extends EmptyTermKind
{
  def unapply(arg: MultiTerm): FastRefOption[EmptyTerm.type] =
    new FastRefOption(EmptyTerm)

  override def cast(x:MultiTerm): EmptyTerm.type = EmptyTerm

}

/*
trait ContradictionTermKind extends MultiTermKind
{
  type In = MultiTerm
  type Out = ContradictionTerm

  def contradiction(x:MultiTerm): ContradictionTerm
}
*/

trait StarTermKind extends NonContradictionTermKind
{
  type In = MultiTerm
  type Out = StarTerm

  @inline final def star(x:MultiTerm): StarTerm = cast(x)


}

trait OrSetTermKind extends NonContradictionTermKind
{
  type In = MultiTerm
  type Out = OrSetTerm

  @inline final def orSet(x:MultiTerm): OrSetTerm = x.asInstanceOf[OrSetTerm]

}

trait AndSetTermKind extends NonContradictionTermKind
{
  type In = MultiTerm
  type Out = AndSetTerm

  @inline final def andSet(x:MultiTerm): AndSetTerm = x.asInstanceOf[AndSetTerm]

}



trait OrElseTermKind extends NonContradictionTermKind
{
  type In = MultiTerm
  type Out = OrElseTerm

  @inline final def orElse(x:MultiTerm): OrElseTerm = x.asInstanceOf[OrElseTerm]

}

trait IfTermKind extends NonContradictionTermKind
{
  type In = MultiTerm
  type Out = IfTerm

  @inline final def guarded(x:MultiTerm): IfTerm = x.asInstanceOf[IfTerm]

}

object IfTermKind extends IfTermKind