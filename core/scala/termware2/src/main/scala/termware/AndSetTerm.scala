package termware

import termware.util.{FastRefOption, SeqSetTerm, SetTerm}

/**
  * This is one non-contradiction term.
  * EmptyCoset is star.
  */
trait AndSetTerm extends SetTerm {

  override def kind: MultiTermKind = AndSetTerm.Kind

  override def dropExternalContext(): AndSetTerm with NoExternalContext

}



object AndSetTerm  {

  def createPoints(subterms:PointTerm*): MultiTerm =
    new SeqAndSetTerm(subterms)

  object Kind extends AndSetTermKind


}


class SeqAndSetTerm(terms: Seq[MultiTerm]) extends SeqSetTerm with AndSetTerm with NoExternalContext
{
  override val seq: IndexedSeq[MultiTerm] = terms.toIndexedSeq


  /**
    * Apply term to this in current context.
    * Form expression (this term) and then apply reduction rules.
    * By condition, return emtpy if term was not applicable to this.
    *
    * @param term
    * @return
    */
  override def apply(term: PointTerm): MultiTerm = {
    var r: MultiTerm = EmptyTerm
    // TODO: while find, applyAll ?
    seq.find{ t =>
      t match {
        case IsArrowTerm(a) =>
          val u = a.left.unify(term)
          if (u.isExists()) {
            r=a.right.subst(u)
            true
          } else {
            false
          }
        case _ => false
      }
    }
    r
  }

  /**
    * Substitute term in context
    *
    * @param context in which we substitute one.
    * @return
    */
  override def subst(context: MultiTerm): MultiTerm = {
    new SeqAndSetTerm(seq.map(_.subst(context)))
  }

  /**
    * Unify
    *
    * @return
    */
  override def unify(arg: MultiTerm): MultiTerm = ???

  override def or(x: MultiTerm): MultiTerm = ???

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    new SeqAndSetTerm(seq.map(_.pushInternalContext(context)))
  }

  override def dropExternalContext(): AndSetTerm with NoExternalContext = this

}

class AndSetTermInExternalContext(term: AndSetTerm with NoExternalContext, externContext:MultiTerm)
   extends TermInExternalContext(term,externContext) with AndSetTerm
{

  override def mapReduce[A](map: MultiTerm => A)(reduce: (A, A) => A)(zero: => A): A = {
    term.mapReduce(map)(reduce)(zero)
  }

  override def members(): Seq[MultiTerm] = {
    term.members().map(_.addExternalContext(externContext))
  }

  override def unify(x: MultiTerm): MultiTerm = {
    // TODO: move up
    ifExternalContext(x.externalContext()){ joined =>
      TermInExternalContext(term unify x.dropExternalContext(),joined)
    }
  }

  override def or(x: MultiTerm): MultiTerm = {
    val join = externalContext and x.externalContext()
    if (join.isEmpty()) {
      OrSetTerm._fromSeq(Seq(this,x))
    } else if (join == externalContext && join == x.externalContext()){
      TermInExternalContext(term or x,join)
    } else {
      OrElseTerm(TermInExternalContext(term or x,join),OrSetTerm._fromSeq(Seq(this,x)))
    }
  }

  override def dropExternalContext(): AndSetTerm with NoExternalContext = term

}

object AndSetTermInExternalContext
{

  def apply(term: AndSetTerm with NoExternalContext, externContext: MultiTerm): AndSetTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      term
    } else {
      new AndSetTermInExternalContext(term, externContext)
    }
  }

}