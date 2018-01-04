package termware

import scala.collection.immutable.Queue

sealed trait ContextTerm
{

  this: MultiTerm =>

  def updateContext(ctx:MultiTerm)

}

abstract class ContextMultiTerm(origin:MultiTerm, context:MultiTerm) extends MultiTerm with ContextTerm {

  override def name: Name = origin.name

  override def cardinality: Int = origin.cardinality



  override def resolve(x: PointTerm): MultiTerm = {
    val cx = context.apply(x)
    if (cx.isEmpty) {
      context.resolve(x)
    }else{
      cx
    }
  }

  override def unify(x: MultiTerm): MultiTerm = ???

  override def subst(x: MultiTerm): MultiTerm = ???



}

trait EmptyContext extends MultiTerm
{

  override def path = Queue(name.toTerm)

  override def context: MultiTerm = EmptyTerm

  override def uncontext: MultiTerm with EmptyContext = this

  override def resolve(x: PointTerm): MultiTerm = EmptyTerm

  override def narrow(x: PointTerm): PointTerm = x

  override def inside(ctx: MultiTerm): MultiTerm = this.in(ctx)

}

object ContextMultiTerm
{

  def create(v:MultiTerm, c:MultiTerm): MultiTerm =
    v.multiKind match {
      case MultiKind.Empty(e) => e
      case MultiKind.Contradiction(e) => e
      case r =>
        c.multiKind match {
          case MultiKind.Empty(e) => v
          case MultiKind.Contradiction(e) => e
          case _ =>
            r match {
              case MultiKind.Set(s) => s.mapOr(_.in(c))
              case _ => ???
            }
        }

    }

}


abstract class ContextPointTerm(pt:PointTerm,val context:MultiTerm) extends ContextMultiTerm(pt,context) with PointTerm
{
  override def arity: Int = pt.arity

  override def name: Name = pt.name

  override def path: Queue[PointTerm] = context.resolve(KernelNames.nameTerm).multiKind match {
    case MultiKind.Empty(e) => Queue(name.toTerm)
    case _ => context.path :+ name.toTerm
  }

  override def in(ctx: MultiTerm): MultiTerm =
          ContextPointTerm.create(pt,ctx.or(context))

  override def inside(context: MultiTerm): MultiTerm =
    ContextPointTerm.create(pt.uncontext,SeqOrTerm.create(context,pt.context))

  override def subst(x: MultiTerm): MultiTerm = {
    // TODO: think, are we need subst in context
    pt.subst(x).in(context.subst(x))
  }


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








class ContextSeqOrTerm(origin:SeqOrTerm with EmptyContext, override val context: MultiTerm)
  extends ContextMultiTerm(origin,context) with SeqOrTerm
{

  override def head: MultiTerm =
      origin.head.in(context)

  override def tail: MultiTerm =
     origin.tail.in(context)

  override def inside(ctx: MultiTerm): MultiTerm =
     map( _.inside(ctx) )


  override def multiKind: MultiKind = MultiKind.SeqOr(this)

  override def updateContext(ctx: MultiTerm): Unit = ???

  override def path: Queue[PointTerm] = ???

  override def uncontext: SeqOrTerm with EmptyContext = origin

  override def in(ctx: MultiTerm): MultiTerm = ???

  override def narrow(x: PointTerm): PointTerm = ???

  override def or(other: MultiTerm): MultiTerm = ???

  override def and(other: MultiTerm): MultiTerm = ???

  override def eval(other: MultiTerm): MultiTerm = ???

  override def map(f: (MultiTerm) => MultiTerm): MultiTerm =
      {
        val mf = f(head)
        val mt = tail.multiKind match {
          case MultiKind.SeqOr(s) => s map f
          case MultiKind.Empty(e) => e // impossible
          case _ => f(tail)
        }
        mt.multiKind match {
          case MultiKind.Empty(e) => mf
          case _ => mt
        }
      }

  override def unify(x: MultiTerm): MultiTerm = ???

  override def subst(x: MultiTerm): MultiTerm = ???

  /**
    * true, if <code>x</code> is compatible with <code>this</code> as context.
    *
    * @param x
    * @return
    */
  override def check(x: PointTerm): Boolean =
    find(_.check(x)).multiKind match {
      case MultiKind.Empty(e) => false
      case MultiKind.Point(pt) => ScalaCompability.asBoolean(pt)
      case _ => false
    }

}