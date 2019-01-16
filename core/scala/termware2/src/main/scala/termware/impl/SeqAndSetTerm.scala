package termware.impl

import termware._
import termware.util.SeqSetTerm

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
  override def termApply(term: PointTerm): MultiTerm = {
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
  override def unify(arg: MultiTerm): MultiTerm = {
    mapReduce(_.unify(arg))( _ and _ )(StarTerm.U)
  }

  override def or(x: MultiTerm): MultiTerm = {
    // TODO: think about tagged unify ?
    val u = (this unify x)
    if (u.isEmpty()) {
      OrSetTerm._fromSeq(Seq(this,x))
    } else if (u.externalContext().isStar()) {
      u
    } else {
      OrSetTerm._fromSeq(Seq(this,x))
    }
  }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    new SeqAndSetTerm(seq.map(_.pushInternalContext(context)))
  }

  override def dropExternalContext(): AndSetTerm with NoExternalContext = this

}
