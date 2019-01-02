package termware

import cats.effect.IO

trait MultiTerm
{


  /**
    * Kind of term.
    * Used for marching by cases of kind, when we want form compiler exhausive case analysis)
    *  (this is workaround for restricting case analysis to sealed traits in scala)
    * @return kind of term (sealed case class)
    */
  def kind:MultiTermKind


  /**
    * resolve term in context of this
    * @param term
    * @return
    */
  def resolve(term:MultiTerm): MultiTerm

  /**
    * Apply term to this in current context.
    * Form expression (this term) and then apply reduction rules.
    * By condition, return emtpy if term was not applicable to this.
    * @param term
    * @return
    */
  def apply(term:PointTerm): MultiTerm


  /**
    * Substitute term in context
    * @param context in which we substitute one.
    * @return
    */
  def subst(context:MultiTerm):MultiTerm

  /**
    * Unify
    * @return
    */
  def unify(arg: MultiTerm): MultiTerm

  @inline
  final def <> (arg: MultiTerm): MultiTerm = {
    this unify arg
  }


  def isEmpty(): Boolean = (kind == EmptyTermKind)

  def isExists(): Boolean = !( isEmpty() )

  def ifExists(t:MultiTerm): MultiTerm  =
    if (t.isEmpty() ) {
      t
    } else this

  def isStar(): Boolean = kind.isInstanceOf[StarTermKind]

  def or(x:MultiTerm): MultiTerm

  def and(x:MultiTerm): MultiTerm = {
    val u = unify(x)
    if (u.isEmpty()) {
      EmptyTerm
    } else {
      val e = u.externalContext()
      u.dropExternalContext().subst(e)
    }
  }

  def orElse(x: MultiTerm): MultiTerm = {
    OrElseTerm(this,x)
  }

  def cond(x: PointTerm): MultiTerm = {
    IfTerm(this,x)
  }

  def arrow(right: MultiTerm): MultiTerm =
    ArrowTerm(this,right)

  final def --> (right: MultiTerm): MultiTerm =
    arrow(right)

  def substExternalContext(): MultiTerm = {
    subst(externalContext).dropExternalContext
  }

  def externalContext(): MultiTerm

  def dropExternalContext(): MultiTerm with NoExternalContext


  /**
    * Set external context
    * @param context
    * @return
    */
  def setExternalContext(context: MultiTerm): MultiTerm

  def -:(context: MultiTerm): MultiTerm =
    setExternalContext(context)

  def :- (arg: MultiTerm): MultiTerm =
    arg.setExternalContext(this)

  def addExternalContext(context: MultiTerm): MultiTerm = {
    setExternalContext(externalContext() and context)
  }

  def pushInternalContext(context: MultiTerm): MultiTerm

  final def ^ (context: MultiTerm) = pushInternalContext(context)

  /**
    * Check, if other external context
    * @param otherEC
    * @param f: function, wich accept joined external context
    * @return EmptyText, if externContext and otherEC invompatible, f(joinContext) otherwise
    */
  def ifExternalContext(otherEC: MultiTerm)(f: MultiTerm => MultiTerm): MultiTerm = {
    val jointEc = externalContext() and otherEC
    jointEc.kind match {
      case k: EmptyTermKind => EmptyTerm
      case _ => f(jointEc)
    }
  }


}

