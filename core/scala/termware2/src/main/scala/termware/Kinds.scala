package termware


import termware.util.FastRefOption

sealed trait MultiTermKind
object MultiTermKind
{


}


sealed trait PointTermKind extends MultiTermKind
{
  def pointTerm(x:MultiTerm): PointTerm = {
    x.asInstanceOf[PointTerm]
  }
}

trait AtomTermKind extends PointTermKind
{
  def atomTerm(x:PointTerm):AtomTerm
}

trait PrimitiveTermKind extends PointTermKind
{
  def primitive(x:PointTerm): PrimitiveTerm[_]
}

trait SingletonNameKind extends PointTermKind
{
  def singletonName: SingletonName
}

trait ArrowTermKind extends PointTermKind
{
  def arrow(x:PointTerm): ArrowTerm
}

trait StructuredTermKind extends PointTermKind
{
  def structured(x:PointTerm): StructuredTerm
}

trait EmptyTermKind extends MultiTermKind

object EmptyTermKind extends EmptyTermKind
{
  def unapply(arg: MultiTerm): FastRefOption[EmptyTerm.type] =
    new FastRefOption(EmptyTerm)
}

trait ContradictionTermKind extends MultiTermKind
{
  def contradiction(x:MultiTerm): ContradictionTerm
}

trait StarTermKind extends MultiTermKind
{
  def star(x:MultiTerm): StarTerm
}