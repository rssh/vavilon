package termware


import termware.util.FastRefOption

sealed trait MultiTermKind
{
  type Out <: MultiTerm
  type In <: MultiTerm

  def cast(x:In):Out =
    x.asInstanceOf[Out]

}


sealed trait PointTermKind extends MultiTermKind
{
  type In <: MultiTerm
  type Out <: PointTerm

  @inline
  def pointTerm(x:MultiTerm): PointTerm = {
    x.asInstanceOf[PointTerm]
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
  override type Out = PrimitiveTerm[_]

  def primitive(x:PointTerm): PrimitiveTerm[_]

}



trait SingletonNameKind extends PointTermKind
{
  override type In = PointTerm
  override type Out = SingletonName

  def singletonName(): SingletonName

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

trait EmptyTermKind extends MultiTermKind
{
  type In = MultiTerm
  type Out = EmptyTerm.type
}

object EmptyTermKind extends EmptyTermKind
{
  def unapply(arg: MultiTerm): FastRefOption[EmptyTerm.type] =
    new FastRefOption(EmptyTerm)

  override def cast(x:MultiTerm ): EmptyTerm.type = EmptyTerm

}

trait ContradictionTermKind extends MultiTermKind
{
  type In = MultiTerm
  type Out = ContradictionTerm

  def contradiction(x:MultiTerm): ContradictionTerm
}

trait StarTermKind extends MultiTermKind
{
  type In = MultiTerm
  type Out = StarTerm

  @inline final def star(x:MultiTerm): StarTerm = cast(x)


}