package termware.util

/**
  * Value-like option for use in pattern matching.
  * @param value
  * @tparam T
  */
class FastRefOption[T <: AnyRef](val value:T) extends AnyVal {

  def isDefined: Boolean = !(value eq null)

  def get(): T = value

}
