package termware

import termware.util.{FastRefOption, NameIndexed, SetTermOps}

sealed trait MultiTerm extends MultiTermOps

final object EmptyTerm extends MultiTerm with AndSetTerm with OrSetTerm with EmptyTermOps

trait StarTerm extends MultiTerm with AndSetTerm with StarTermOps

sealed trait SetTerm extends MultiTerm with SetTermOps

trait OrSetTerm extends SetTerm with OrSetTermOps

trait AndSetTerm extends SetTerm with AndSetTermOps

sealed trait PointTerm extends MultiTerm with PointTermOps

trait PrimitiveTerm[T] extends PointTerm with PrimitiveTermTOps[T]
trait StructuredTerm extends PointTerm with StructuredTermOps

sealed trait Name extends PointTerm with NameOps with Ordered[Name]
trait SingletonName extends PointTerm with Name with SingletonNameOps
@specialized(Byte,Int,Long,Double,Char)
trait PrimitiveName[T] extends Name with PrimitiveNameOps[T]

trait AtomTerm extends PointTerm  with AtomTermOps with Name


trait ArrowTerm extends PointTerm with ArrowTermOps


trait OrElseTerm extends MultiTerm with OrElseTermOps

trait IfTerm extends MultiTerm with IfTermOps


object StarTerm {

  object Kind extends StarTermKind

  val U = ContextLessStarTerm

}


object OrSetTerm {

  @inline final def create(subterms: MultiTerm*):MultiTerm = {
     OrSetTermOps.create(subterms: _*)
  }

  object Kind extends OrSetTermKind

}



object AndSetTerm  {

  @inline final def createPoints(subterms:PointTerm*): MultiTerm =
    AndSetTermOps.createPoints(subterms: _*)

  object Kind extends AndSetTermKind


}



object OrElseTerm
{

  @inline final def apply(frs: MultiTerm, snd: MultiTerm): MultiTerm =
    OrElseTermOps.apply(frs,snd)

  object Kind extends OrElseTermKind

}


object IfTerm {
  @inline def apply(value:MultiTerm, condition:PointTerm): MultiTerm =
    IfTermOps(value,condition)

  object Kind extends IfTermKind


}

object PrimitiveTerm
{

  object Kind extends PrimitiveTermKind {
    override def primitive(x: PointTerm): PrimitiveTerm[_] = {
      x.asInstanceOf[PrimitiveTerm[_]]
    }
  }


}

object StructuredTerm
{

  object Kind extends StructuredTermKind

  @inline final def create(nameTerm: AtomTerm, indexes: NameIndexed[MultiTerm]): StructuredTerm =
    StructuredTermOps.create(nameTerm,indexes)


}

object AtomTerm
{

  object Kind extends AtomTermKind {
    override def atomTerm(x: PointTerm): AtomTerm = x.asInstanceOf[AtomTerm]
  }

  //TODO: delegate to AtomTermOps
  def apply(sname:String) = {
    ContextLessAtomTerm(sname)
  }

  def unapply(arg: AtomTerm): FastRefOption[AtomTerm] = {
    new FastRefOption(arg)
  }
}


object ArrowTerm  {


  @inline final def apply(left: MultiTerm, right: MultiTerm): MultiTerm = {
     ArrowTermOps.apply(left,right)
  }

  object Kind extends ArrowTermKind {
    override def arrow(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
    override def cast(x: PointTerm): ArrowTerm = x.asInstanceOf[ArrowTerm]
  }

}
