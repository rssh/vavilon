package termware

import scala.collection.immutable.Queue


trait StarTerm extends MultiTerm
{
  override def name: Name = StarName

  override def cardinality: Int = Int.MaxValue

  override def multiKind: MultiKind = MultiKind.Star(this)

  /**
    * '*' in context is a contradiction.
    */
  override def check(x: PointTerm): Boolean = false

  override def apply(other: MultiTerm): MultiTerm = EmptyTerm

}


/**
  * Plain Star term without context.
  */
case object DefaultStarTerm extends StarTerm with EmptyContext {

  override def context: MultiTerm = EmptyTerm

  override def uncontext: StarTerm with EmptyContext = this

  override def in(ctx: MultiTerm): MultiTerm =
      ctx.multiKind match {
        case MultiKind.Contradiction(e) => e
        case MultiKind.Empty(e) => this
        case _ => ContextStarTerm(ctx)
      }


  override def or(other:MultiTerm) = this
  override def and(other:MultiTerm) = other

  override def unify(x: MultiTerm): MultiTerm = x

  override def subst(x: MultiTerm): MultiTerm =
    x.multiKind match {
      case MultiKind.Empty(e) => this
      case MultiKind.Contradiction(x) => x
      case MultiKind.Set(s) => s.mapOr(this subst _)
      case MultiKind.SeqOr(s) => s.map( this subst _)
      case MultiKind.Star(s) => this
      case MultiKind.Point(pt) =>
        pt.pointKind match {
          case PointKind.Arrow(a) => a.right
          case _ => this  // TODO: rethink,
        }
    }


}

case class ContextStarTerm(context:MultiTerm) extends  ContextMultiTerm(DefaultStarTerm,context) with StarTerm with MultiTermImpl[ContextStarTerm] {

  override def path: Queue[PointTerm] = context.resolve(KernelNames.nameTerm) match {
    case EmptyTerm => Queue(name.toTerm)
    case x => x.path :+ name.toTerm
  }

  override def in(ctx: MultiTerm): MultiTerm =
    ctx.multiKind match {
      case MultiKind.Empty(e) => this
      case MultiKind.Contradiction(e) => e
      case _ => ContextStarTerm.create(this in ctx)
    }

  override def inside(ctx: MultiTerm): MultiTerm =
     DefaultStarTerm in (ctx orElse context)

  override def uncontext: StarTerm with EmptyContext = DefaultStarTerm


  //(*|c) or (p|cp) = (*|c or sel.p -> cp)
  override def or(other: MultiTerm) = {
    other.multiKind match {
      case MultiKind.Empty(e) => this
      case MultiKind.Contradiction(e) => e
      case MultiKind.Point(p) =>
        if (p.context.isEmpty) {
          this
        } else {
          ContextStarTerm(context or ArrowTerm.create(p.uncontext, p.context))
        }
      case MultiKind.Set(s) =>
        ContextStarTerm(context or s.mapOr {
          x => if (x.context.isEmpty) EmptyTerm else ArrowTerm.create(x.uncontext, x.context)
        })
      case MultiKind.SeqOr(s) =>
       ContextStarTerm(context or s.map(x => if (x.context.isEmpty) EmptyTerm else ArrowTerm.create(x.uncontext,x.context) ) )
      case MultiKind.Star(s) =>
        if (s.context.isEmpty) {
          s
        } else {
          ContextStarTerm(context or s.context)
        }
    }
  }

  override def and(other:MultiTerm):MultiTerm = {
    other.multiKind match {
      case MultiKind.Empty(e) => e
      case MultiKind.Contradiction(e) => e
      case MultiKind.Point(p) => p.inside(context)
      case MultiKind.SeqOr(e) => e.map( this and _ )
      case MultiKind.Set(s) => s.mapOr(this and _)
      case MultiKind.Star(s) => ContextStarTerm.create( context and s.context )  // ???


    }
  }


  override def updateContext(ctx: MultiTerm): Unit = ???

  override def narrow(x: PointTerm): PointTerm = ???

}

object ContextStarTerm
{
  def create(ctx:MultiTerm):MultiTerm =
    ctx.multiKind match {
      case MultiKind.Empty(e) => DefaultStarTerm
      case MultiKind.Contradiction(e) => e
      case _ => ContextStarTerm(ctx)
    }
}
