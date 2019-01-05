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

  type Self <: EmptyContext

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


