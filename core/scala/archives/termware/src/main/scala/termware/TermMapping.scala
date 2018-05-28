package termware


import scala.util.Try

trait TermMapping[T]
{

  def toScala(x:MultiTerm):Try[T]

  def fromScala(x:T):Try[MultiTerm]

}
