package termware




/**
  * Multiset, { a, b}
  * Can be a representation of a rulset, if each of members is arrow.
  * So,
  *   { a -> b, c -> d }, so have hight-level optimized operations for this,
  */
trait SetTerm extends MultiTerm {


  /**
    * select part which comply pattern
    * @param pattern
    * @return
    */
  def select(pattern: MultiTerm): MultiTerm

  /**
    * select left parts, which comply pattern, retun unification of left parts and
    */
  def selectLeft(pattern:MultiTerm): MultiTerm


  def mapReduce(mapf: PointTerm => MultiTerm)(reduce:(MultiTerm, MultiTerm) => MultiTerm)


  def add(otherTerm: MultiTerm): MultiTerm


  def members(): Seq[MultiTerm]


}


// TODO: implement.
//  emptyseq == EmptyTerm
abstract class SeqSetTerm(seq: Seq[PointTerm]) extends SetTerm
{

  override def select(pattern: MultiTerm): MultiTerm = ???


  /**
    * select left parts, which comply pattern
    */
  override def selectLeft(pattern: MultiTerm): MultiTerm = ???
  override def mapReduce(mapf: PointTerm => MultiTerm)(reduce: (MultiTerm, MultiTerm) => MultiTerm): Unit = ???
  override def add(otherTerm: MultiTerm): MultiTerm = ???
  override def members(): Seq[MultiTerm] = ???

  override def kind: MultiTermKind = ???
}

  //TODO: Implement
// -- empty-arrows == EmptyTerm
abstract class MapArrowsSetTerm(arrows: Map[PointTerm,MultiTerm]) extends SetTerm

// TODO: Implement
abstract class NetworkSetTerm(byArity: Map[Int,MapArrowsSetTerm]) extends SetTerm
