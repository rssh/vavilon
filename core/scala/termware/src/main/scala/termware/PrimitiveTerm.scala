package termware



trait PrimitiveTerm[T] extends PointTerm with EmptyContext
{
  def value:T

  def primitiveTypeIndex: Int

  def ordering: Ordering[T]

  def termConstructor(x:T): PrimitiveTerm[T]

  def valueAs[S<:T]:S = value.asInstanceOf[S]

  override def name: Name

  override def arity: Int = 0

  override def pointKind: PointKind = PointKind.Primitive(this)

  override def context: MultiTerm = EmptyTerm

  override def uncontext: PrimitiveTerm[T] with EmptyContext = this

  override def in(ctx: MultiTerm): MultiTerm = this

  override def orPoint(other: PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Primitive(otherPrimitive) =>
             if (otherPrimitive.primitiveTypeIndex != primitiveTypeIndex) {
               SetTerm.create(this,otherPrimitive)
             } else {
                if (value == otherPrimitive.value) {
                  this
                } else {
                  SetTerm.create(this,otherPrimitive)
                }
             }
      case _ => SetTerm.create(this,other)
    }

  override def andPoint(other: PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Primitive(otherPrimitive) =>
        if (otherPrimitive.primitiveTypeIndex != primitiveTypeIndex) {
          EmptyTerm
        } else {
          if (value == otherPrimitive.value) {
            this
          } else {
            EmptyTerm
          }
        }
      case _ => EmptyTerm
    }

  override def eval(other: MultiTerm): MultiTerm = ???


  override def pointUnify(other:PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Primitive(otherPrimitive) =>
                 if (otherPrimitive.primitiveTypeIndex != this.primitiveTypeIndex) {
                   EmptyTerm
                 } else {
                   val asT = otherPrimitive.asInstanceOf[PrimitiveTerm[T]]
                   if (value == asT.value) {
                     this
                   } else {
                     EmptyTerm
                   }
                   //primitiveUnify(otherPrimitive.asInstanceOf[PrimitiveTerm[T]])
                 }
      case _ => EmptyTerm
    }

  //def primitiveUnify(other: PrimitiveTerm[T]): MultiTerm

  override def subst(x: MultiTerm): MultiTerm = this

}



abstract class PrimitiveTermImpl[S<:PrimitiveTermImpl[S,T],T](val value: T) extends PrimitiveTerm[T] with PointTermImpl[S] {

  this: S =>

  override def name: Name = PrimitiveName(value)(ops)

  def ops: PrimitiveTermOps[S,T]

  def primitiveTypeIndex: Int = ops.primitiveTypeIndex

  def ordering: Ordering[T] = ops.ordering

  def termConstructor(x:T): PrimitiveTerm[T] = ops.termConstructor(x)


}


abstract class PrimitiveTermOps[S<:PrimitiveTermImpl[S,T],T]
{

  final type Primitive = T
  type PName = PrimitiveName[T]

  val primitiveTypeIndex: Int

  def ordering: Ordering[T]

  def termConstructor(x:T): PrimitiveTerm[T]


}


case class ByteTerm(v:Byte) extends PrimitiveTermImpl[ByteTerm,Byte](v)
{
  def ops = ByteTermOps

}

object ByteTermOps extends PrimitiveTermOps[ByteTerm,Byte]
{

  override final val primitiveTypeIndex: Int = 1

  override def ordering: Ordering[Byte] = implicitly[Ordering[Byte]]

  override def termConstructor(x: Byte): ByteTerm = ByteTerm(x)

}

case class ShortTerm(v:Short) extends PrimitiveTermImpl[ShortTerm,Short](v)
{
  def ops = ShortTermOps
}

object ShortTermOps extends PrimitiveTermOps[ShortTerm,Short]
{
  override final val primitiveTypeIndex: Int = 2

  override def ordering: Ordering[Short] = implicitly[Ordering[Short]]

  override def termConstructor(x: Short): ShortTerm = ShortTerm(x)

}

case class IntTerm(v:Int) extends PrimitiveTermImpl[IntTerm,Int](v)
{
  def ops = IntTermOps
}

object IntTermOps extends PrimitiveTermOps[IntTerm,Int]
{
  override final val primitiveTypeIndex: Int = 3

  override def ordering: Ordering[Int] = implicitly[Ordering[Int]]

  override def termConstructor(x: Int): IntTerm = IntTerm(x)
}

case class LongTerm(v:Long) extends PrimitiveTermImpl[LongTerm,Long](v)
{
  def ops = LongTermOps
}


object LongTermOps extends PrimitiveTermOps[LongTerm,Long]
{
  override final val primitiveTypeIndex: Int = 4

  override def ordering: Ordering[Long] = implicitly[Ordering[Long]]

  override def termConstructor(x: Long): LongTerm = LongTerm(x)
}

case class DoubleTerm(v:Double) extends PrimitiveTermImpl[DoubleTerm,Double](v)
{
  def ops = DoubleTermOps
}


object DoubleTermOps extends PrimitiveTermOps[DoubleTerm,Double]
{
  override val primitiveTypeIndex: Int = 8

  override def ordering: Ordering[Double] = implicitly[Ordering[Double]]

  override def termConstructor(x: Double): DoubleTerm = DoubleTerm(x)
}

case class BigDecimalTerm(v:BigDecimal) extends PrimitiveTermImpl[BigDecimalTerm,BigDecimal](v)
{
  def ops = BigDecimalTermOps
}

object BigDecimalTermOps extends PrimitiveTermOps[BigDecimalTerm,BigDecimal]
{
  override val primitiveTypeIndex: Int = 9

  override def ordering: Ordering[BigDecimal] = implicitly[Ordering[BigDecimal]]

  override def termConstructor(x: BigDecimal) = BigDecimalTerm(x)
}

//TODO: add unsigned types


case class CharTerm(v:Char) extends PrimitiveTermImpl[CharTerm,Char](v)
{
  def ops = CharTermOps
}


object CharTermOps extends PrimitiveTermOps[CharTerm,Char]
{
  override final val primitiveTypeIndex: Int = 17

  override def ordering: Ordering[Char] = implicitly[Ordering[Char]]

  override def termConstructor(x: Char): CharTerm = CharTerm(x)
}


case class StringTerm(v:String) extends PrimitiveTermImpl[StringTerm,String](v)
{
  def ops = StringTermOps
}


object StringTermOps extends PrimitiveTermOps[StringTerm,String]
{
  override final val primitiveTypeIndex: Int = 18

  override def ordering: Ordering[String] = implicitly[Ordering[String]]

  override def termConstructor(x: String) = StringTerm(x)
}

case class OpaqueTerm(v:Array[Byte]) extends PrimitiveTermImpl[OpaqueTerm,Array[Byte]](v)
{
  def ops = OpaqueTermOps
}

object OpaqueTermOps extends PrimitiveTermOps[OpaqueTerm,Array[Byte]]
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

  override def termConstructor(x: Array[Byte]) = OpaqueTerm(x)
}

case class BooleanTerm(v:Boolean) extends PrimitiveTermImpl[BooleanTerm,Boolean](v)
{
  def ops = BooleanTermOps
}

object BooleanTermOps extends PrimitiveTermOps[BooleanTerm,Boolean]
{
  override val primitiveTypeIndex: Int = 20

  override def ordering: Ordering[Boolean] = implicitly[Ordering[Boolean]]

  override def termConstructor(x: Boolean): PrimitiveTerm[Boolean] = BooleanTerm(x)
}
