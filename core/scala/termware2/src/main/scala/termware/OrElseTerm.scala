package termware

import termware.util.FastRefOption

trait OrElseTerm extends MultiTerm {


  override def kind = OrElseTerm.Kind

  /**
    * Find term which satisficy predicate or empty term if not found.
    * @param p
    * @return
    */
  def firstNotEmpty(p: MultiTerm => MultiTerm): MultiTerm = {
    firstMapped(p)(! _.isEmpty() )(EmptyTerm)
  }


  def firstMapped[A](f:MultiTerm => A)(p:A => Boolean)(default: =>A): A


  /**
    * Create orElseterm which each term is mapped.
    * @param f
    * @return
    */
  def map(f:MultiTerm => MultiTerm): MultiTerm

  def orElse(t:MultiTerm): MultiTerm

  override def unify(arg: MultiTerm): MultiTerm = {
    firstMapped{_ unify arg}(_.isExists())(EmptyTerm)
  }

  def or(t: MultiTerm): MultiTerm = {
    t.kind match {
      case k: EmptyTermKind => this
      case k: StarTermKind => t
      case k: OrSetTermKind => k.orSet(t) or t
      case _ => OrSetTerm._fromSeq(Seq(this,t))
    }
  }

  override def dropExternalContext(): OrElseTerm with NoExternalContext

}

// TODO: seq ?
class ContextlessOrElseTerm(frs:MultiTerm, snd: MultiTerm) extends OrElseTerm with NoExternalContext
{

  override def resolve(term: MultiTerm): MultiTerm = map(_.resolve(term))

  override def apply(term: PointTerm): MultiTerm = {
    frs.apply(term) match {
      case EmptyTerm => snd.apply(term)
      case other => other
    }
  }

  def map(f: MultiTerm => MultiTerm): MultiTerm = {
    f(frs) orElse f(snd)
  }

  def firstMapped[A](f:MultiTerm => A)(p:A => Boolean)(default: =>A): A = {
    val c = f(frs)
    if (p(c)) c else {
      snd match {
         case OrElseTerm(snd) => snd.firstMapped(f)(p)(default)
         case _ => val cs = f(snd)
           if (p(cs)) {
             cs
           } else default
      }
    }
  }


  override def orElse(t:MultiTerm): MultiTerm = new ContextlessOrElseTerm(frs,snd orElse t)

  override def subst(context: MultiTerm): MultiTerm = {
    new ContextlessOrElseTerm(frs.subst(context),snd.subst(context))
  }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    map(_.pushInternalContext(context))
  }

  override def dropExternalContext(): OrElseTerm with NoExternalContext = this

}

class OrElseTermInExternalContext(t: OrElseTerm with NoExternalContext, externContext:MultiTerm) extends TermInExternalContext(t,externContext) with OrElseTerm {

  override def firstMapped[A](f: MultiTerm => A)(p: A => Boolean)(default: => A): A = {
    t.firstMapped(x => f(TermInExternalContext(x,externContext)))(p)(default)
  }

  override def map(f: MultiTerm => MultiTerm): MultiTerm = {
    t.map(x => f(TermInExternalContext(x,externContext)))
  }

  override def dropExternalContext(): OrElseTerm with NoExternalContext = t

}

object OrElseTermInExternalContext {

  def apply(t: OrElseTerm with NoExternalContext, externContext: MultiTerm): MultiTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      t
    } else {
      new OrElseTermInExternalContext(t, externContext)
    }
  }

}

object OrElseTerm
{

  def apply(frs: MultiTerm, snd: MultiTerm): MultiTerm = {
    frs.kind match {
      case k: EmptyTermKind => snd
      case k: StarTermKind => frs
      case otherK =>
        snd.kind match {
          case k: EmptyTermKind => frs
          case other =>
            new ContextStarTerm(frs,snd)
        }
    }
  }

  //def fromSeq()

  object Kind extends OrElseTermKind

  def unapply(arg: MultiTerm): FastRefOption[OrElseTerm] = {
    arg.kind match {
      case k:OrElseTermKind => FastRefOption(k.orElse(arg))
      case _ =>  FastRefOption.empty
    }
  }

}