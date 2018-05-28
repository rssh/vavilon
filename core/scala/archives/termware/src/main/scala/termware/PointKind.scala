package termware

sealed trait PointKind

object PointKind
{

  case class Atom(value:AtomTerm) extends PointKind
  case class Primitive[T](value: PrimitiveTerm[T]) extends PointKind
  case class Structured(value: StructuredTerm) extends PointKind
  case class Sequence(value: SequenceTerm) extends PointKind
  case class Arrow(value:ArrowTerm) extends PointKind


}
