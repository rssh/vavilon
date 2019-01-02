package termware

import termware.util.{FastRefOption, SeqSetTerm, SetTerm}


/**
  * Multiset, { a, b}
  * Can be a representation of a rulset, if each of members is arrow.
  * So,
  *   { a -> b, c -> d }, so have hight-level optimized operations for this,
  */
trait OrSetTerm extends SetTerm {


  def apply(term: PointTerm): MultiTerm

  override def subst(context: MultiTerm): MultiTerm = {
    mapReduce(_.subst(context))(_ or _)(EmptyTerm)
  }

  override def unify(arg: MultiTerm): MultiTerm = {
    mapReduce(_ unify arg)(_ or _)(EmptyTerm)
  }

  override def dropExternalContext(): OrSetTerm with NoExternalContext

}


object OrSetTerm
{

  // Default constructor
  def create(subterms: MultiTerm*):MultiTerm = {
     val r: MultiTerm = EmptyTerm
     subterms.foldLeft(r)(_ or _)
  }

  def createPoints(subterms:PointTerm*): MultiTerm =
    new SeqOrSetTerm(subterms)

  def _fromSeq(subterms: Seq[MultiTerm]):MultiTerm =
    new SeqOrSetTerm(subterms)

  def _fromMap(map:Map[Name,MultiTerm]):OrSetTerm = {
    val seq = map.foldLeft(IndexedSeq[PointTerm]()){ case (s,(n,v)) =>
      val a = PlainArrowTerm(n,v)
      s :+ a
    }
    new SeqOrSetTerm(seq)
  }

  object Kind extends OrSetTermKind


}


// TODO: implement.
//  emptyseq == EmptyTerm
// This is the most simply unoptimized form, which will be changed later.
class SeqOrSetTerm(inSeq: Seq[MultiTerm]) extends OrSetTerm with SeqSetTerm with NoExternalContext
{

  val seq = inSeq.toIndexedSeq

  override def apply(term: PointTerm): MultiTerm = {
    val s0: MultiTerm = EmptyTerm
    seq.foldLeft(s0) { (s, t) =>
      t match {
        case IsArrowTerm(a) =>
          val u = a.left.unify(term)
          if (u.isEmpty()) {
            // skip
            s
          } else {
            s or a.right.subst(u.externalContext())
          }
        case _ => s
      }
    }
  }


  override def or(otherTerm: MultiTerm): MultiTerm = {
    otherTerm.kind match {
      case k:PointTermKind =>
        addOrElement(k.pointTerm(otherTerm))
      case k:EmptyTermKind =>
        this
      case k:StarTermKind =>
        // !!!TODO:  add to context or.
        otherTerm
      case k: OrSetTermKind =>
        val otherSet = k.orSet(otherTerm)
        val s0: MultiTerm = this
        otherSet.members().foldLeft(s0){ (s,e) =>
           s match {
             case IsOrSet(x) => x or e
             case _ => s
           }
        }
      case _: AndSetTermKind | _:IfTermKind =>
        addOrElement(otherTerm)
      case k: OrElseTermKind =>
        k.cast(otherTerm).map(p => this or p)
    }
  }


  private def addOrElement(otherTerm: MultiTerm): MultiTerm = {
    var i = 0
    var found = false
    var nseq = seq
    while(i < seq.length  && !found) {
      val c = seq(i)
      val cu = c and otherTerm
      if (!cu.isEmpty()) {
        found = true
        if (cu == otherTerm) {
           nseq = seq.patch(i,Seq(otherTerm),1)
        } else if (cu != c) {
           nseq = seq.patch(i,Seq(seq(i),otherTerm),2)
        }
      }
      i += 1
    }
    if (!found) {
      new SeqOrSetTerm(seq :+ otherTerm)
    } else if (seq eq nseq) {
      this
    } else {
      new SeqOrSetTerm(nseq)
    }
  }

  override def members(): Seq[MultiTerm] = seq

  override def kind: MultiTermKind = OrSetTerm.Kind

  override def resolve(term: MultiTerm): MultiTerm = {
    seq.view.map(_.resolve(term)).filter(!_.isEmpty()).fold(EmptyTerm)(_ or _)
  }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    new SeqOrSetTerm(seq.map(_.pushInternalContext(context)))
  }

  override def dropExternalContext(): OrSetTerm with NoExternalContext = this

}

class OrSetInExternalContext(term: OrSetTerm with NoExternalContext, externContext: MultiTerm) extends TermInExternalContext(term,externContext) with OrSetTerm {

  override def kind: MultiTermKind = OrSetTerm.Kind

  override def mapReduce[A](map: MultiTerm => A)(reduce: (A, A) => A)(zero: => A): A = {
    term.mapReduce(x => map(TermInExternalContext(x,externContext)))(reduce)(zero)
  }

  override def members(): Seq[MultiTerm] = term.members()

  override def or(x: MultiTerm): MultiTerm = {
     if (externContext == x.externalContext()) {
       val nterm = term or x.dropExternalContext()
       TermInExternalContext(nterm,externContext)
     } else {
       OrSetTerm._fromSeq(Seq(this,x))
     }
  }

  override def dropExternalContext(): OrSetTerm with NoExternalContext =
    term

}

object OrSetInExternalContext
{

  def apply(term: OrSetTerm with NoExternalContext, externContext: MultiTerm): OrSetTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      term
    } else {
      new OrSetInExternalContext(term, externContext)
    }
  }

  def unapply(arg: MultiTerm): FastRefOption[(OrSetTerm with NoExternalContext, MultiTerm)] = {
    arg.kind match {
      case k:OrSetTermKind => val orSet = k.orSet(arg)
        if (orSet.externalContext().isStar()) {
          FastRefOption.empty
        }  else {
          FastRefOption(orSet.dropExternalContext(),orSet.externalContext())
        }
      case _ => FastRefOption.empty
    }
  }

}



object IsOrSet
{

  def unapply(arg: MultiTerm): FastRefOption[OrSetTerm] = {
    arg.kind match {
      case k:OrSetTermKind => new FastRefOption[OrSetTerm](k.orSet(arg))
      case _ => new FastRefOption[OrSetTerm](null)
    }
  }

}

  //TODO: Implement
// -- empty-arrows == EmptyTerm
abstract class MapArrowsSetTerm(arrows: Map[PointTerm,MultiTerm]) extends OrSetTerm

// TODO: Implement
abstract class NetworkSetTerm(byArity: Map[Int,MapArrowsSetTerm]) extends AndSetTerm
