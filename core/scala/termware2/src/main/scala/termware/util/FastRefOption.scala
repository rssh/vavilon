package termware.util

/**
  * Value-like option for use in pattern matching.
  * @param value
  * @tparam T
  */
class FastRefOption[+T <: AnyRef](val value:T) extends AnyVal {

  def isEmpty: Boolean = !(value eq null)

  def get(): T = value

}


object FastRefOption
{

  @inline
  final def apply[T <: AnyRef](x:T) = new FastRefOption[T](x)

  val empty: FastRefOption[Null] = new FastRefOption(null)

}