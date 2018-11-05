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


  /**
    * Term, resolved in own context.
    *
    * @return
    */
  override def resolved(): MultiTerm =
    IfTermKind(value.resolved(),condition)

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

    def thisContext = ArrowName(this )
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

class PointIfTerm(override val value: PointTerm, override val condition: PointTerm) extends IfTerm(value, )