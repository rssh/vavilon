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

sealed trait FastRefBooleanOption
{

  def isEmpty: Boolean

  def get(): Boolean


}

object FastRefBooleanOption
{

  case object Empty extends FastRefBooleanOption
  {
    override def isEmpty: Boolean = true

    override def get(): Boolean = throw new NoSuchElementException()
  }

  case object True extends FastRefBooleanOption
  {
    override def isEmpty: Boolean = false
    override def get(): Boolean = true
  }

  case object False extends FastRefBooleanOption
  {
    override def isEmpty: Boolean = false
    override def get(): Boolean = false
  }

  def fromBoolean(value: Boolean): FastRefBooleanOption = {
    if (value) True else False
  }


}