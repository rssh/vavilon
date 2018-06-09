package termware

import termware.util.NameIndexed

trait StructuredTerm extends PointTerm {

  type Self <: StructuredTerm

  override def kind: StructuredTermKind = StructuredTerm


  def get(name: Name): Option[MultiTerm]


  /**
    * @param i : 0 < i < arity
    * @return
    */
  def get(i:Int):Option[MultiTerm]

  def names(): IndexedSeq[Name]

  def subterms(): IndexedSeq[MultiTerm]

  def namedSubterms(): NameIndexed[MultiTerm]

  def newNamedSubterms(newIndexes: NameIndexed[MultiTerm]): Self

}

object StructuredTerm extends StructuredTermKind
{

  override def cast(x: PointTerm): StructuredTerm = x.asInstanceOf[StructuredTerm]


}

case class PlainStructuredTerm(override val name:Name,
                               indexes:NameIndexed[MultiTerm],
                               override val context: MultiTerm = EmptyTerm) extends StructuredTerm
{
  override type Self = PlainStructuredTerm

  override def get(name: Name): Option[MultiTerm] = indexes.get(name)

  /**
    * @param i : 0 < i < arity
    * @return
    */
  override def get(i: Int): Option[MultiTerm] = indexes.get(i)

  override def names(): IndexedSeq[Name] = indexes.names

  override def subterms(): IndexedSeq[MultiTerm] = indexes.values

  override def namedSubterms(): NameIndexed[MultiTerm] = indexes

  override def arity: Int = indexes.size

  override def newNamedSubterms(newIndexes: NameIndexed[MultiTerm]): Self =
    PlainStructuredTerm(name,newIndexes)

}

