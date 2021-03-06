package termware

import termware.util.FastRefOption

trait PointTermOps extends MultiTermOps {

  this: PointTerm =>

  def name: Name

  def arity: Int

  override def kind: PointTermKind

  override def termApply(term: PointTerm): MultiTerm = EmptyTerm

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
        case k: OrElseTermKind => k.cast(x).firstMapped(unify(_))(!_.isEmpty()) {
          EmptyTerm
        }
        case k: AndSetTermKind =>
          val andSet = k.andSet(x)
          andSet.mapReduce(ct => unify(ct))(_ and _)(StarTerm.U)
        case k: IfTermKind =>
          val ifTerm = k.guarded(nx)
          unify(ifTerm.value) match {
            case EmptyTerm => EmptyTerm
            case other => val ec = other.externalContext()
              // subst leave same kind, but this is not in types.
              val newCondition = ifTerm.condition.subst(ec).asInstanceOf[PointTerm]
              IfTerm(other.dropExternalContext(),newCondition).setExternalContext(ec)
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
      case k:PointTermKind => orPoint(k,k.pointTerm(x))
      case k:EmptyTermKind => this
      case k:OrSetTermKind => k.orSet(x) or this
      case k:AndSetTermKind => OrSetTermOps._fromSeq(Seq(this,x))
      case k:OrElseTermKind => k.cast(x).map(_ or x)
      case k:StarTermKind => k.cast(x)
      case k:IfTermKind => OrSetTermOps._fromSeq(Seq(this,x))
    }
  }

  def orPoint(k:PointTermKind, x:PointTerm): MultiTerm = {
    OrSetTermOps.createPoints(this,x)
  }

  override def dropExternalContext(): PointTerm with NoExternalContext


  lazy val thisContext = ArrowTerm(KernelNames.thisName,this)

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
