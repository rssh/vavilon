package termware

/**
  * trait for pattern matching over different variants of the MultiTerm.
  */
sealed trait MultiKind
{
  def x: MultiTerm
}


object MultiKind
{


  case class  Star(x:StarTerm) extends MultiKind
  case class  Set(x:SetTerm) extends MultiKind
  case class  SeqOr(x:SeqOrTerm) extends MultiKind
  case class  Point(x:PointTerm) extends MultiKind
  case class  Empty(x:EmptyTerm) extends MultiKind
  case class  Contradiction(x:ContradictionTerm)extends MultiKind

}
