package termware

import termware.util.FastRefOption

trait PointTerm extends MultiTerm {

  def name: Name

  def arity: Int

  override def kind: PointTermKind

  override lazy val resolved: MultiTerm = resolve(this)

  override def apply(term: PointTerm): MultiTerm = EmptyTerm

  override def unify(x: TermInContext): TermInContext = {
    x.term.kind match {
      case k: EmptyTermKind => x
      case k: StarTermKind =>
               val checkExpression = k.cast(x.term).resolve(KernelNames.checkName)
               if (checkExpression.isEmpty()) {
                 this ^^ x.context
               } else {
                 val condition = KernelLanguage.evalCheck(checkExpression,x.term, thisContext compatibleOr x.context )
                 ???
               }
      case k: ContradictionTermKind => x
      case k: PointTermKind => pointUnify(k,x.asInstanceOf[InContext[PointTerm]])
      case k: SetTermKind => setUnify(k,x)
      case k: OrElseTermKind => k.cast(x.term).firstMapped(_.unify(x))(! _.term.isEmpty()){
        val failureContext = KernelLanguage.contextWithFailure(x.context,"none was found")
        TermInContext(EmptyTerm,failureContext)
      }
    }
  }

  def pointUnify(ptk: PointTermKind, u: InContext[PointTerm]):TermInContext

  def setUnify(k:SetTermKind, u:InContext[MultiTerm]): TermInContext = {
     val setTerm = k.set(u.term)
     // TODO:  recheck or, maybe create two copy for two different subst.
     setTerm.mapReduce(ct => ct.unify(u))(_ or _)(u.copy(term = EmptyTerm))
  }


  override def and(x: MultiTerm): MultiTerm = {
    x.kind match {
      case k: StarTermKind => val sx = k.star(x)
        val check = sx.resolve(KernelNames.checkName)
        if (check.isExists()) {
          // will be changed to  If (this.check() , ...
          val checkResult = KernelLanguage.evalCheck(check,this,ArrowTerm(KernelNames.thisName,this))
          checkResult.term match {
            case BooleanTerm(value) =>
              if (value) {
                this
              } else {
                EmptyTerm
              }
            case other =>
              // TODO: will be changed
              // in theory we should add check here.
              this
          }
        } else {
          this
        }
      case k: EmptyTermKind => k.cast(x)
      case k: ContradictionTermKind => x
      case k: SetTermKind => val selected = k.set(x).selectAll(TermInContext(this,EmptyTerm))
        val s0: MultiTerm = EmptyTerm
        selected.foldLeft(s0){ case (s,e) =>
          val ce = e.term.subst(e.context)
          (this and ce) or s
        }
      case k: PointTermKind => pointAnd(k,k.pointTerm(x))
      case k: OrElseTermKind =>
        k.cast(x).map(this and _)
    }
  }

  def pointAnd(ptk:PointTermKind, x:PointTerm):MultiTerm = {
    pointUnify(ptk,InContext(x,EmptyTerm)).term.resolved()
  }

  def or(x:MultiTerm): MultiTerm = {
    x.kind match {
      case k:PointTermKind => SetTerm.create(this,k.pointTerm(x))
      case k:EmptyTermKind => this
      case k:SetTermKind => k.set(x) or x
      case k:OrElseTermKind => k.cast(x).map(_ or x)
      case k:StarTermKind => k.cast(x)
      case k:ContradictionTermKind => x
    }
  }

  override def compatibleOr(x: MultiTerm): MultiTerm = {
     this and x  // will be overrided in Arrow,
  }

  def generateCheckExpression(): MultiTerm = ???

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
