package termware

import scala.reflect.runtime.universe

trait PrimitiveTerm[T] extends PointTerm
{
  def value:T

  def primitiveTypeIndex: Int

  def ordering: Ordering[T]

  def termConstructor(x:T): PrimitiveTerm[T]

  def valueAs[S<:T]:S = value.asInstanceOf[S]

  override def arity: Int = 0

}


trait BasePrimitiveTerm[T] extends PrimitiveTerm[T] with PrimitiveName[T]
{

  override def kind: PrimitiveTermKind = BasePrimitiveTerm

  override def name: Name = this

  override def termConstructor(x:T): BasePrimitiveTerm[T]


}



object BasePrimitiveTerm  extends PrimitiveTermKind
{
  override def primitive(x: PointTerm): BasePrimitiveTerm[_] = {
    x.asInstanceOf[BasePrimitiveTerm[_]]
  }
}


abstract class BasePrimitiveTermImpl[S<:BasePrimitiveTermImpl[S,T],T](val value: T) extends BasePrimitiveTerm[T] {

  this: S =>

  type Self = BasePrimitiveTermImpl[S,T]

  def ops: PrimitiveTermOps[S,T]

  def primitiveTypeIndex: Int = ops.primitiveTypeIndex

  def ordering: Ordering[T] = ops.ordering

  def termConstructor(x:T): BasePrimitiveTerm[T] = ops.termConstructor(x)


}


abstract class PrimitiveTermOps[S<:BasePrimitiveTermImpl[S,T],T]
{

  final type Primitive = T
  type PName = PrimitiveName[T]

  val primitiveTypeIndex: Int

  def ordering: Ordering[T]

  def termConstructor(x:T): BasePrimitiveTerm[T]

}


case class ByteTermBase(v:Byte) extends BasePrimitiveTermImpl[ByteTermBase,Byte](v)
{

  override val ops: PrimitiveTermOps[ByteTermBase,Byte] = ByteTermOps

}

object ByteTermOps extends PrimitiveTermOps[ByteTermBase,Byte]
{

  override final val primitiveTypeIndex: Int = 1

  override def ordering: Ordering[Byte] = implicitly[Ordering[Byte]]

  override def termConstructor(x: Byte): ByteTermBase = ByteTermBase(x)

}

case class ShortTermBase(v:Short) extends BasePrimitiveTermImpl[ShortTermBase,Short](v)
{
  def ops = ShortTermOps
}

object ShortTermOps extends PrimitiveTermOps[ShortTermBase,Short]
{
  override final val primitiveTypeIndex: Int = 2

  override def ordering: Ordering[Short] = implicitly[Ordering[Short]]

  override def termConstructor(x: Short): ShortTermBase = ShortTermBase(x)

}

case class IntTermBase(v:Int) extends BasePrimitiveTermImpl[IntTermBase,Int](v)
{
  def ops = IntTermOps
}

object IntTermOps extends PrimitiveTermOps[IntTermBase,Int]
{
  override final val primitiveTypeIndex: Int = 3

  override def ordering: Ordering[Int] = implicitly[Ordering[Int]]

  override def termConstructor(x: Int): IntTermBase = IntTermBase(x)
}

case class LongTermBase(v:Long) extends BasePrimitiveTermImpl[LongTermBase,Long](v)
{
  def ops = LongTermOps
}


object LongTermOps extends PrimitiveTermOps[LongTermBase,Long]
{
  override final val primitiveTypeIndex: Int = 4

  override def ordering: Ordering[Long] = implicitly[Ordering[Long]]

  override def termConstructor(x: Long): LongTermBase = LongTermBase(x)
}

case class DoubleTermBase(v:Double) extends BasePrimitiveTermImpl[DoubleTermBase,Double](v)
{
  def ops = DoubleTermOps
}


object DoubleTermOps extends PrimitiveTermOps[DoubleTermBase,Double]
{
  override val primitiveTypeIndex: Int = 8

  override def ordering: Ordering[Double] = implicitly[Ordering[Double]]

  override def termConstructor(x: Double): DoubleTermBase = DoubleTermBase(x)
}

case class BigDecimalTermBase(v:BigDecimal) extends BasePrimitiveTermImpl[BigDecimalTermBase,BigDecimal](v)
{
  def ops = BigDecimalTermOps
}

object BigDecimalTermOps extends PrimitiveTermOps[BigDecimalTermBase,BigDecimal]
{
  override val primitiveTypeIndex: Int = 9

  override def ordering: Ordering[BigDecimal] = implicitly[Ordering[BigDecimal]]

  override def termConstructor(x: BigDecimal) = BigDecimalTermBase(x)
}

//TODO: add unsigned types


case class CharTermBase(v:Char) extends BasePrimitiveTermImpl[CharTermBase,Char](v)
{
  def ops = CharTermOps
}


object CharTermOps extends PrimitiveTermOps[CharTermBase,Char]
{
  override final val primitiveTypeIndex: Int = 17

  override def ordering: Ordering[Char] = implicitly[Ordering[Char]]

  override def termConstructor(x: Char): CharTermBase = CharTermBase(x)
}


case class StringTermBase(v:String) extends BasePrimitiveTermImpl[StringTermBase,String](v)
{
  def ops = StringTermOps
}


object StringTermOps extends PrimitiveTermOps[StringTermBase,String]
{
  override final val primitiveTypeIndex: Int = 18

  override def ordering: Ordering[String] = implicitly[Ordering[String]]

  override def termConstructor(x: String) = StringTermBase(x)
}

case class OpaqueTermBase(v:Array[Byte]) extends BasePrimitiveTermImpl[OpaqueTermBase,Array[Byte]](v)
{
  def ops = OpaqueTermOps
}

object OpaqueTermOps extends PrimitiveTermOps[OpaqueTermBase,Array[Byte]]
{

  override final val primitiveTypeIndex: Int = 19

  override val ordering: Ordering[Array[Byte]] = (x: Array[Byte], y: Array[Byte]) => {
    val c0 = x.length - y.length
    if (c0 != 0) {
      c0
    } else {
      var i = 0
      var c = 0
      while (i < x.length && c != 0) {
        c = x(i) - y(i)
        i += 1
      }
      c
    }
  }

  override def termConstructor(x: Array[Byte]) = OpaqueTermBase(x)
}

case class BooleanTermBase(v:Boolean) extends BasePrimitiveTermImpl[BooleanTermBase,Boolean](v)
{
  def ops = BooleanTermOps

}

object BooleanTermOps extends PrimitiveTermOps[BooleanTermBase,Boolean]
{
  override val primitiveTypeIndex: Int = 20

  override def ordering: Ordering[Boolean] = implicitly[Ordering[Boolean]]

  override def termConstructor(x: Boolean): BasePrimitiveTerm[Boolean] = BooleanTermBase(x)
}


final case class ContextPrimitiveTerm[T](base: BasePrimitiveTerm[T],context: MultiTerm) extends PrimitiveTerm[T]
{
  override def value: T = base.value

  override def primitiveTypeIndex: Int = base.primitiveTypeIndex

  override def ordering: Ordering[T] = base.ordering

  override def termConstructor(x: T): PrimitiveTerm[T] = ContextPrimitiveTerm(base.termConstructor(x),context)

  override def name: Name = base.name

  override def kind: PointTermKind = ContextPrimitiveTerm
}


object ContextPrimitiveTerm extends PrimitiveTermKind
{
  override def primitive(x: PointTerm): PrimitiveTerm[_] = {
    x.asInstanceOf[ContextPrimitiveTerm[_]]
  }
}