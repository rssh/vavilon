package termware


sealed trait Name extends Ordered[Name]
{

  type Carrier

  def typeIndex: Int

  def compare(that: Name): Int =
  {
    val cmp = typeIndex - that.typeIndex
    if (cmp !=0) cmp else compareSameTypeIndex(that)
  }

  def compareSameTypeIndex(that: Name): Int

  def carrier: Carrier

}

abstract class StringLikeName(val value:String) extends Name
{

   type Carrier = String

   override def carrier: String = value

   def compareSameTypeIndex(that: Name) =
     value compare that.asInstanceOf[StringLikeName].value

}

@specialized(Byte,Int,Long,Double,Char)
final case class PrimitiveName[T](val value: T)(implicit descriptor: PrimitiveDescriptor[T]) extends Name
{

  override type Carrier = T

  override def carrier: Carrier = value

  override def typeIndex: Int = descriptor.primitiveTypeIndex

  def compareSameTypeIndex(that: Name) =
    descriptor.ordering.compare(value, that.asInstanceOf[PrimitiveName[T]].value)

}


/**
  * Singleton name, each name have unique type-index. (example: Universum)
  * @param typeIndex
  */
abstract class SingletonName(override val typeIndex: Int) extends Name
{
  override final type Carrier = Unit

  override final def carrier: Carrier = ()

  override final def compareSameTypeIndex(that: Name): Int = 0
}

final case class AtomName(s:String) extends StringLikeName(s)
{
  override def typeIndex: Int = TypeIndexes.ATOM
}

final object SeqName extends SingletonName(TypeIndexes.SEQ)
final object SetName extends SingletonName(TypeIndexes.SET)
final object StarName extends SingletonName(TypeIndexes.STAR)
final object UnitName extends SingletonName(TypeIndexes.UNIT)
final object ErrorName extends SingletonName(TypeIndexes.ERROR)
final object ArrowName extends SingletonName(TypeIndexes.ARROW)

object TypeIndexes
{

  // 1-128: reserved for primitives [see primitive descriptor ]
  final val ATOM = 129

  final val SEQ  = 200
  final val SET  = 201
  final val UNIT = 202
  final val ARROW = 203
  final val STAR = 254
  final val ERROR =255

}

