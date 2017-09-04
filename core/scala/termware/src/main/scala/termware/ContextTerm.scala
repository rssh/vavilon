package termware

sealed trait ContextTerm

abstract class ContextMultiTerm(origin:MultiTerm, context:MultiTerm) extends MultiTerm {

  override def name: Name = origin.name

  override def cardinality: Int = origin.cardinality

  override def multiKind: MultiKind =
     origin.multiKind match {
       case x@MultiKind.Empty(e) => x
       case x@MultiKind.Error(e) => x
       case x@MultiKind.Star(s) =>
                  s match {
                    case DefaultStarTerm => MultiKind.Star(ContextStarTerm(context))
                    case _ => ??? // impossible. If we have ContextStarTerm here, it means part of term come to us unnormalized
                  }
       case x@MultiKind.Set(s) => // impossible,
                                  s.mapMerge(_.updateContext(context)).multiKind
       case x@MultiKind.Point(pt) =>
                          pt match {
                            case ContextPointTerm(x,xc) => ??? //impossible()
                            case _ => ContextPointTerm.create(pt,context).multiKind
                          }

     }


}

trait EmptyContext extends MultiTerm
{
  override def context: MultiTerm = EmptyTerm

  override def uncontext: MultiTerm with EmptyContext = this
}

object ContextMultiTerm
{

  def create(v:MultiTerm, c:MultiTerm): MultiTerm =
    v.multiKind match {
      case MultiKind.Empty(e) => e
      case MultiKind.Error(e) => e
      case r =>
        c.multiKind match {
          case MultiKind.Empty(e) => v
          case MultiKind.Error(e) => e
          case _ =>
            r match {
              case MultiKind.Set(s) => s.mapMerge(_.updateContext(c))
              case _ => ???
            }
        }

    }

}


abstract class ContextPointTerm(pt:PointTerm,val context:MultiTerm) extends ContextMultiTerm(pt,context) with PointTerm
{
  override def arity: Int = pt.arity

  override def name: Name = pt.name

  override def updateContext(ctx: MultiTerm): MultiTerm =
          ContextPointTerm.create(pt,ctx.mergeAsLeftContext(context))

  override def mergeAsLeftContext(other: MultiTerm): MultiTerm =
          other.multiKind match {
            case MultiKind.Empty(e) => this
            case MultiKind.Error(e) => e
            case MultiKind.Set(s) => ???
            case MultiKind.Point(pt) => if (pt == other) {
              pt
            } else {
              pointMergeAsLeftContext(pt)
            }
            case MultiKind.Star(s) => this   // ??? !!
          }

  def pointMergeAsLeftContext(other: PointTerm): MultiTerm

  override def selectAsContextPattern(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeAnd(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeOr(other: MultiTerm): MultiTerm = ???

  override def selectAsLeftPattern(other: MultiTerm): MultiTerm = ???
}

object ContextPointTerm
{

  def create(pt: PointTerm,context: MultiTerm): MultiTerm =
     pt.pointKind match {
       case PointKind.Atom(a) => ContextAtomTerm.create(a,context)
       case PointKind.Sequence(s) => ContextSequenceTerm.create(s,context)
       case PointKind.Structured(s) => ContextStructuredTerm.create(s,context)
       case PointKind.Arrow(s) => ContextArrowTerm.create(s,context)
       case PointKind.Primitive(p) => p
     }

  def unapply(arg: PointTerm): Option[(PointTerm, MultiTerm)] =
    arg.context match {
      case EmptyTerm => None
      case ctx => arg.pointKind match {
        case PointKind.Primitive(p) => None
        case PointKind.Arrow(a) => Some((a,ctx))
        case PointKind.Atom(a) => Some((a,ctx))
        case PointKind.Structured(s) => Some((s,ctx))
        case PointKind.Sequence(s) => Some((s,ctx))
      }
    }

}





case class ContextStructuredTerm(origin: StructuredTerm with EmptyContext, override val context: MultiTerm) extends ContextPointTerm(origin,context) with StructuredTermImpl[ContextStructuredTerm]
{
  override def pointMergeAsLeftContext(other: PointTerm): MultiTerm = ???

  override def get(n: Name): MultiTerm =
     ContextMultiTerm.create(origin.get(n),context)

  override def subterms(): IndexedSeq[MultiTerm] =
      origin.subterms().map(ContextMultiTerm.create(_,context))

  override def uncontext: StructuredTerm with EmptyContext = origin
}

object ContextStructuredTerm
{

  def create(origin: StructuredTerm, context:MultiTerm): MultiTerm =
    context.multiKind match {
      case MultiKind.Empty(e) => origin
      case MultiKind.Error(e) => e
      case x => if (origin.context.isEmpty) {
        ContextStructuredTerm(origin.uncontext,context)
      } else {
        origin.updateContext(context)
      }
    }

}




