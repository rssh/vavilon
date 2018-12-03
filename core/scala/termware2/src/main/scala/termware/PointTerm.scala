package termware

import termware.util.FastRefOption

trait PointTerm extends MultiTerm {

  def name: Name

  def arity: Int

  override def kind: PointTermKind

  override def apply(term: PointTerm): MultiTerm = EmptyTerm

  override def unify(x: MultiTerm): MultiTerm = {
    val joinContext = x.externalContext() and externalContext()
    if (joinContext.isEmpty()) {
      EmptyTerm
    } else {
      val nx = x.setExternalContext(joinContext)
      nx.kind match {
        case k: EmptyTermKind => x
        case k: StarTermKind => this
        case k: PointTermKind => pointUnify(k, k.pointTerm(nx))
        case k: OrSetTermKind => orSetUnify(k, x)
        case k: OrElseTermKind => k.cast(x).firstMapped(_.unify(x))(!_.isEmpty()) {
          EmptyTerm
        }
      }
    }
  }

  def pointUnify(ptk: PointTermKind, u: PointTerm): MultiTerm

  def orSetUnify(k:OrSetTermKind, u:MultiTerm): MultiTerm = {
     val setTerm = k.orSet(u)
     // TODO:  recheck or, maybe create two copy for two different subst.
     setTerm.mapReduce(ct => ct.unify(u))(_ or _)(EmptyTerm)
  }



  def or(x:MultiTerm): MultiTerm = {
    x.kind match {
      case k:PointTermKind => OrSetTerm.createPoints(this,k.pointTerm(x))
      case k:EmptyTermKind => this
      case k:OrSetTermKind => k.orSet(x) or this
      case k:OrElseTermKind => k.cast(x).map(_ or x)
      case k:StarTermKind => k.cast(x)
    }
  }

  lazy val thisContext = ArrowTerm(KernelNames.thisName,this)

}

object PointTerm
{

}

object IsPointTerm
{

  def unapply(x:MultiTerm):FastRefOption[PointTerm] = {
    x.kind match {
      case xk:PointTermKind =>
          FastRefOption(xk.pointTerm(x))
      case _ => FastRefOption(null)
    }
  }

}
