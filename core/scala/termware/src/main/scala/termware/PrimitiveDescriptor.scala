package termware

trait PrimitiveDescriptor[T] {

  final type Primitive = T
  type PName = PrimitiveName[T]

  val primitiveTypeIndex: Int

  def ordering: Ordering[T]

}

object BytePrimitiveDescriptor extends PrimitiveDescriptor[Byte]
{
  override final val primitiveTypeIndex: Int = 1

  override def ordering: Ordering[Byte] = implicitly[Ordering[Byte]]
}

object ShortPrimitiveDescriptor extends PrimitiveDescriptor[Short]
{
  override final val primitiveTypeIndex: Int = 2

  override def ordering: Ordering[Short] = implicitly[Ordering[Short]]
}

object IntPrimitiveDescriptor extends PrimitiveDescriptor[Int]
{
  override final val primitiveTypeIndex: Int = 3

  override def ordering: Ordering[Int] = implicitly[Ordering[Int]]
}

object LongPrimitiveDescriptor extends PrimitiveDescriptor[Long]
{
  override final val primitiveTypeIndex: Int = 4

  override def ordering: Ordering[Long] = implicitly[Ordering[Long]]
}

//TODO: add unsigned types

object CharPrimitiveDescriptor extends PrimitiveDescriptor[Char]
{
  override final val primitiveTypeIndex: Int = 17

  override def ordering: Ordering[Char] = implicitly[Ordering[Char]]
}

object StringPrimitiveDescriptor extends PrimitiveDescriptor[String]
{
  override final val primitiveTypeIndex: Int = 18

  override def ordering: Ordering[String] = implicitly[Ordering[String]]
}



