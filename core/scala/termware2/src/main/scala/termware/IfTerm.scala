package termware


abstract class IfTerm(val value: MultiTerm, val condition:PointTerm) extends MultiTerm {

  /**
    * Kind of term.
    * Used for marching by cases of kind, when we want form compiler exhausive case analysis)
    * (this is workaround for restricting case analysis to sealed traits in scala)
    *
    * @return kind of term (sealed case class)
    */
  override def kind: MultiTermKind = IfTermKind


  override lazy val resolved: MultiTerm = {
    val resolvedValue = value.resolved
    val resolvedCondition = condition.resolved
    resolvedCondition match {
      case k:PointTermKind =>
        // TODO: mixEval
        val ct = k.pointTerm(condition)
        lazy val r: IfTerm = new IfTerm(resolvedValue,ct) {
          override lazy val resolved: IfTerm = r
        }
        r
      case k:ContradictionTermKind =>
        k.contradiction(resolvedCondition)
      case x => // Impossible
        // TODO: rethink or add data to context
        this
    }
  }

  /**
    * resolve term in context of this
    *
    * @param term
    * @return
    */
  override def resolve(term: MultiTerm): MultiTerm =
    value.resolve(term)


  /**
    * Substitute term in context
    *
    * @param context in which we substitute one.
    * @return
    */
  override def subst(context: MultiTerm): MultiTerm = {
    val resolved = value.subst(context)
    val resolvedContext = condition.subst(context)

    ???
    //def resolvedContext = ArrowTerm(AtomName("this"), resolved)
    //PointIfTerm(resolved, resolvedContext)
  }

  override def apply(argument: PointTerm): MultiTerm = {
    value.apply(argument)
  }

  /**
    * Unify
    *
    * @return
    */
  override def unify(arg: TermInContext): TermInContext = ???

  override def or(x: MultiTerm): MultiTerm = ???

  override def compatibleOr(x: MultiTerm): MultiTerm = ???
}

