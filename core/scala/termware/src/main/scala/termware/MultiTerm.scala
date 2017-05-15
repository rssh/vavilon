package termware

sealed trait MergePolicy
object MergePolicy
{
  case object LeftFirst extends MergePolicy
  case object RightFirst extends MergePolicy
  case object Unificate  extends MergePolicy
  case object Disallow  extends MergePolicy
}

sealed trait MultiTerm
{

  def name: Name

  def cardinality: Int

  def point(i:Int): Option[PointTerm]

  def points: IndexedSeq[PointTerm]

  def merge(x1: MultiTerm, mp: MergePolicy): MultiTerm
    = operations.Merge(this,x1,mp)

  def normalize(): MultiTerm = this

}



sealed trait PointTerm extends MultiTerm
{
  def name: Name

  def cardinality: Int = 1

  def point(i:Int): Option[PointTerm] =
    i match {
      case 0 => Some(this)
      case _ => None
    }

  lazy val points: IndexedSeq[PointTerm] = IndexedSeq(this)

  def arity: Int

  def left: MultiTerm

  def right: MultiTerm

  def subterm(n:Name):Option[MultiTerm]

  def subterm(i:Int):Option[MultiTerm]

  def subterms(): IndexedSeq[MultiTerm]

}

sealed trait OrdinaryTerm extends PointTerm
{
  def left: MultiTerm = EmptyTerm
  def right: MultiTerm = this

  override def normalize() = this

}

sealed trait ZeroArityTerm extends OrdinaryTerm
{
  def name: Name

  override def arity: Int = 0

  override def subterm(i: Int): Option[MultiTerm] = None

  override def subterm(n: Name): Option[MultiTerm] = None

  override def subterms(): IndexedSeq[MultiTerm] = IndexedSeq.empty


}

class PrimiveTerm[T](val value:T, descriptor: PrimitiveDescriptor[T]) extends ZeroArityTerm
{
  override val name: Name = PrimitiveName(value)(descriptor)

  override def normalize() = this
}

case class AtomTerm(override val name: Name) extends ZeroArityTerm
{
  override def arity: Int = 0
  override def cardinality: Int = 1
  override def normalize() = this
}

case class SequenceTerm(val values: IndexedSeq[MultiTerm]) extends OrdinaryTerm
{
  override def name = SequenceTerm.name
  override def arity: Int = values.size

  override def subterm(n: Name): Option[MultiTerm] = {
    val i: Int = n.typeIndex match {
      case IntPrimitiveDescriptor.primitiveTypeIndex => n.asInstanceOf[PrimitiveName[Int]].value
      case BytePrimitiveDescriptor.primitiveTypeIndex => n.asInstanceOf[PrimitiveName[Byte]].value.toInt
      case ShortPrimitiveDescriptor.primitiveTypeIndex => n.asInstanceOf[PrimitiveName[Short]].value.toInt
      case _ => -1
    }
    subterm(i)
  }

  override def subterm(i: Int): Option[MultiTerm] =
  {
    if (i >= 0 && i < values.size) { // TODO, think how to eliminate in some cases
      Some(values(i))
    }else{
      None
    }
  }

  override def subterms(): IndexedSeq[MultiTerm] = values
}

object SequenceTerm
{
  final val name = SeqName
}

case class StructuredTerm(name:Name,values: NameIndexed[MultiTerm]) extends OrdinaryTerm
{

  override def arity = values.size

  override def subterm(n: Name): Option[MultiTerm] = values.get(n)

  override def subterm(i: Int): Option[MultiTerm] = values.get(i)

  override def subterms(): IndexedSeq[MultiTerm] = values.values

}

final case class SetTerm(values: IndexedSeq[PointTerm]) extends MultiTerm
{

  override def name: Name = SetName

  override def cardinality: Int = values.size

  override def point(i: Int): Option[PointTerm] =
    if (i >= 0 && i < values.size) {
      Some(values(i))
    } else {
      None
    }

  override def points: IndexedSeq[PointTerm] = values

  override def normalize():MultiTerm =
  {
    if (values.isEmpty)
      EmptyTerm
    else if (values.size == 1) {
      values(0)
    } else this
  }

}

object SetTerm
{
  def create(values: IndexedSeq[PointTerm]): MultiTerm = {
    if (values.isEmpty) {
      EmptyTerm
    } else if (values.size == 1) {
      values(0)
    } else {
      new SetTerm(values)
    }
  }
}

sealed trait ScopedTerm extends MultiTerm
{

  this: MultiTerm =>

  def context: MultiTerm
}

case class ScopedPointTerm(context: MultiTerm, body: PointTerm) extends PointTerm with ScopedTerm
{

  override def cardinality: Int = body.cardinality

  override def name: Name = body.name

  override def point(i: Int): Option[PointTerm] =
    body.point(i) map (t => ScopedPointTerm(context,t))

  override lazy val points: IndexedSeq[PointTerm] =
  {
    body.points.map(p => ScopedPointTerm(context,p))
  }

  override def arity: Int = body.arity

  override def left: MultiTerm = ScopedTerm(context,body.left)

  override def right: MultiTerm = ScopedTerm(context,body.right)

  override def subterm(n: Name): Option[MultiTerm] = body.subterm(n).map(ScopedTerm(context,_))

  override def subterm(i: Int): Option[MultiTerm] = body.subterm(i).map(ScopedTerm(context,_))

  override lazy val subterms: IndexedSeq[MultiTerm] =
      body.subterms.map(ScopedTerm(context, _ ))

  override def normalize(): MultiTerm = ScopedTerm(context,body)

}

case class  ScopedStarTerm(val context: MultiTerm) extends MultiTerm with ScopedTerm
{
  override def name: Name = StarTerm.name

  override def cardinality: Int = StarTerm.cardinality

  override def point(i: Int): Option[PointTerm] = StarTerm.point(i)

  override def points: IndexedSeq[PointTerm] = StarTerm.points

  override def normalize(): MultiTerm = this
}

case object ScopedTerm
{

  def apply(context:MultiTerm,body:MultiTerm):MultiTerm =
  {
    body match {
      case SetTerm(values) => SetTerm(values.map(ScopedPointTerm(context,_)))
      case ScopedTerm(context1,body) => ScopedTerm(context1.merge(context, MergePolicy.LeftFirst),body)
      case EmptyTerm => EmptyTerm
      case x: PointTerm => new ScopedPointTerm(context,x)
      case e: ErrorTerm => e   // TODO: think
      case StarTerm => ScopedStarTerm(context)
    }
  }

  def unapply(arg: ScopedTerm): Option[(MultiTerm,MultiTerm)] =
  {
    arg match {
      case ScopedStarTerm(context) => Some((context,StarTerm))
      case ScopedPointTerm(context,body) => Some((context,body))
    }
  }

}

case class ArrowTerm(left: MultiTerm, right: MultiTerm) extends PointTerm
{
  override def name: Name = ArrowName

  override def arity: Int = 2

  override def subterm(n: Name): Option[MultiTerm] =
  {
    n match {
      case StdNames.source => Some(left)
      case StdNames.destination => Some(right)
      case _ => None
    }
  }

  override def subterm(i: Int): Option[MultiTerm] =
    i match {
      case 0 => Some(left)
      case 1 => Some(right)
      case _ => None
    }

  override def subterms(): IndexedSeq[MultiTerm] = IndexedSeq(left,right)
}

case object EmptyTerm extends MultiTerm
{

  override def name: Name = StarName

  override def cardinality: Int = 0

  override def points: IndexedSeq[PointTerm] = IndexedSeq.empty

  override def point(i: Int): Option[PointTerm] = None

}

case object StarTerm extends MultiTerm
{
  override def name: Name = StarName

  override def cardinality: Int = 0

  override def point(i: Int): Option[PointTerm] = None

  override def points: IndexedSeq[PointTerm] = IndexedSeq.empty
}

case class ErrorTerm(msg:String,ex:Option[Throwable], additional: MultiTerm) extends MultiTerm
{
  override def name: Name = ErrorName

  override def cardinality: Int = 1

  override def point(i: Int): Option[PointTerm] = None

  override def points: IndexedSeq[PointTerm] = ???
}