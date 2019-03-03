package termware

import scala.util.Try

trait ToTerm[-T]
{

  def toTerm(value:T): Try[MultiTerm]

}

trait FromTerm[+T]
{

  def fromTerm(term: MultiTerm): Try[T]

}

trait TermEmbedded[T] extends ToTerm[T] with FromTerm[T]

trait TermEmbeddingImplicits
{

  object IntTermEmbedded extends TermEmbedded[Int]
  {
    override def fromTerm(term: MultiTerm): Try[Int] = ???

    override def toTerm(value: Int): Try[MultiTerm] = ???
  }

  implicit val intTermEmbedded =IntTermEmbedded


}