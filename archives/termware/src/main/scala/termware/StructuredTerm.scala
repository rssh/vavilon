package termware

import cats.{Applicative, Eval, Traverse}

trait StructuredTerm extends PointTerm
{

  type Self <: StructuredTerm

  def get(n:Name): MultiTerm

  def names():IndexedSeq[Name]

  def subterms():IndexedSeq[MultiTerm]

  def namedSubterms():NameIndexed[MultiTerm]

  //def map(f:MultiTerm => MultiTerm): MultiTerm

  override def pointKind: PointKind = PointKind.Structured(this)

  override def uncontext: StructuredTerm with EmptyContext

  def updated(name:Name,value:MultiTerm): StructuredTerm

  def newSubterms(newSubterms: IndexedSeq[MultiTerm]): Self

  def newNamedSubterms(newNamedSubterms: NameIndexed[MultiTerm]): Self

}

object StructuredTerm
{


  object Test
  {
    def unapply(arg: MultiTerm): Option[StructuredTerm] =
          arg.multiKind match {
            case MultiKind.Point(pt) =>
              pt.pointKind match {
                case PointKind.Structured(s) => Some(s)
                case _ => None
              }
            case _ => None
          }
  }

}



case class ContextlessStructuredTerm(name: Name, indexes:NameIndexed[MultiTerm]) extends StructuredTerm with EmptyContext
{

  override type Self = ContextlessStructuredTerm

  override def andPoint(other: PointTerm): MultiTerm =
    other.pointKind match {
      case PointKind.Structured(otherStructurd) =>  andStructured(otherStructurd)
      case other => ContextContradictionTerm.withMessages("message"->"structured term expected")
    }

  def andStructured(other:StructuredTerm): MultiTerm =
  {
    if (other.name != name) {
      ContextContradictionTerm.withMessages("message"->"name mismatch")
    } else {
      // For now, records are 'closed'. Maybe, add context for open  records later
      //  (i.e value in context)
      if (arity != other.arity) {
        ContextContradictionTerm.withMessages("message"->"size mismatch")
      } else {
        val ctx0: MultiTerm = DefaultStarTerm
        val s0: MultiTerm = ContextlessStructuredTerm(name,NameIndexed.empty)
        this.indexes.foldWhile(s0)(! _.isContradiction ) { case (s, (n,v)) =>
          other.get(n).multiKind match {
            case MultiKind.Empty(e) => ContextContradictionTerm.withMessages("message" -> "name $n is not found", "name" -> n.toString)
            case nonEmpty => val c = (nonEmpty.x and v)
              c.multiKind match {
                case MultiKind.Empty(e) => ContextContradictionTerm.withMessages("message" -> s"empty value for $n")
                case MultiKind.Contradiction(c) => c
                case _ => s match {
                  case StructuredTerm.Test(s) => s.updated(n,c)
                  case _ => s
                }
              }
          }
        }
      }
    }
  }


  override def uncontext: StructuredTerm with EmptyContext = this

  override def get(n: Name): MultiTerm =
    indexes.get(n) match {
      case Some(x) => x
      case None => EmptyTerm
    }

  override def names(): IndexedSeq[Name] =
   indexes.names

  override def subterms(): IndexedSeq[MultiTerm] = ???

  override def namedSubterms(): NameIndexed[MultiTerm] = ???

  override def updated(name: Name, value: MultiTerm): StructuredTerm = ???

  override def arity: Int = ???

  override def in(context: MultiTerm): MultiTerm = ???

  override def pointUnify(other: PointTerm): MultiTerm = ???

  override def orPoint(other: PointTerm): MultiTerm = ???

  override def subst(x: MultiTerm): MultiTerm = ???

  override def apply(other: MultiTerm): MultiTerm = ???

  override def newSubterms(newSubterms: IndexedSeq[MultiTerm]): ContextlessStructuredTerm = ???

  override def newNamedSubterms(newNamedSubterms: NameIndexed[MultiTerm]): ContextlessStructuredTerm = ???
}

object ContextlessStructuredTerm
{

  def create(name: Name, nvs: Tuple2[Name,MultiTerm]*):ContextlessStructuredTerm = {
    ContextlessStructuredTerm(name,NameIndexed.fromSeq(nvs))
  }

  def append(s: MultiTerm, n: Name, c: MultiTerm): MultiTerm =
  {
    s.multiKind match {
      case MultiKind.Point(pt) =>
         pt.pointKind match {
           case PointKind.Structured(s) => s.updated(n,c)
           case PointKind.Atom(a) => ContextlessStructuredTerm.create(a.name,(n,c))
           case _ => ContextContradictionTerm.withMessages("message" -> "can't append name and value to structured term")
         }
      case MultiKind.Set(s) => s.mapOr(append(_,n,c))
      case MultiKind.SeqOr(s) => s.map(append(_,n,c))
      case _ => ContextContradictionTerm.withMessages("message"->s"Cam't append (nv) to term ${s}")
    }
  }

}

case class ContextStructuredTerm(origin: StructuredTerm with EmptyContext, override val context: MultiTerm) extends ContextPointTerm(origin,context) with StructuredTerm
{

  type Self = ContextStructuredTerm

  override def get(n: Name): MultiTerm =
    ContextMultiTerm.create(origin.get(n),context)

  override def subterms(): IndexedSeq[MultiTerm] =
    origin.subterms().map(ContextMultiTerm.create(_,context))


  override def apply(other: MultiTerm): MultiTerm = ???

  override def updateContext(ctx: MultiTerm): Unit = ???

  override def narrow(x: PointTerm): PointTerm = ???

  override def names(): IndexedSeq[Name] = ???

  override def namedSubterms(): NameIndexed[MultiTerm] = ???

  override def uncontext: StructuredTerm with EmptyContext = ???

  override def pointUnify(other: PointTerm): MultiTerm = ???

  override def orPoint(other: PointTerm): MultiTerm = ???

  override def updated(name: Name, value: MultiTerm): StructuredTerm = ???

  override def andPoint(other: PointTerm): MultiTerm = ???

  /**
    * Substitute terms
    *
    * @param x - substitution
    * @return such term,
    */
  override def subst(x: MultiTerm): MultiTerm = ???

  override def newSubterms(newSubterms: IndexedSeq[MultiTerm]): ContextStructuredTerm = ???

  override def newNamedSubterms(newNamedSubterms: NameIndexed[MultiTerm]): ContextStructuredTerm = ???
}

object ContextStructuredTerm
{

  def create(origin: StructuredTerm, context:MultiTerm): MultiTerm =
    context.multiKind match {
      case MultiKind.Empty(e) => origin
      case MultiKind.Contradiction(e) => e
      case x => if (origin.context.isEmpty) {
        ContextStructuredTerm(origin.uncontext,context)
      } else {
        origin.in(context)
      }
    }


}


