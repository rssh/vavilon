package termware

trait StarTerm extends MultiTerm
{

  def context: MultiTerm

  override def subst(context: MultiTerm): MultiTerm = this


}

class PlainStarTerm(override val context: MultiTerm) extends StarTerm {

  override def kind: MultiTermKind = StarTerm

  override def apply(term: PointTerm): MultiTerm =
                                context.resolve(term)

  override def resolve(term: MultiTerm): MultiTerm =
                                EmptyTerm

  override def resolved(): MultiTerm = this

  override def unify(u: TermInContext): TermInContext = {
    val check = context.resolve(KernelNames.checkName)
    if (!check.isExists()) {
      u
    } else {
      u.term.kind match {
        case k: EmptyTermKind => TermInContext(EmptyTerm,u.context)
        case k: ContradictionTermKind => u
        case k: SetTermKind => k.set(u.term).mapReduce{
            x => this unify InContext(x,u.context)
          }(_ or _)(TermInContext.empty)
        case k: OrElseTermKind => k.cast(u.term).firstMapped( x=>
             unify(x ^^ u.context)
          )(!_.term.isEmpty())(TermInContext.empty)
        case k: PointTermKind =>
          KernelLanguage.evalCheck(check,u.term,u.context)
        case k: StarTermKind => val uCheck = k.star(u.term).context.resolve(KernelNames.checkName)
          if (uCheck.isEmpty()) {
            u
          } else {
            import KernelLanguage._
            val newCheck = And(check,uCheck)
            constantEvelBoolean(newCheck,this,u.context) match {
              case None =>
                val t = new PlainStarTerm(ArrowTerm(KernelNames.checkName,newCheck))
                InContext(t,u.context)
              case Some(v) =>
                if (v) {
                  TermInContext(StarTerm.U, u.context)
                } else {
                  EmptyTerm ^^ EmptyTerm
                }
            }
          }
      }
    }
  }

  override def and(x: MultiTerm): MultiTerm = {
    val u = unify(TermInContext(x,EmptyTerm))
    u.term.subst(u.context)
  }

  override def or(x: MultiTerm): MultiTerm = {
    x.kind match {
      case k: EmptyTermKind => this
      case k: ContradictionTermKind => x
      case k: PointTermKind =>
        val check = resolve(KernelNames.checkName);
        if (check.isEmpty()) {
          // TODO:  or contexts ?
           this
        } else {
           // TODO: try apply check, mb it's always true
           // val pointCheck = generateCheckExpression(k.pointTerm());
        }
    }
  }

}


object StarTerm extends StarTermKind {

  val U = new PlainStarTerm(EmptyTerm)


}



