package termware

import termware.util.FastRefOption


/**
  * Multiset, { a, b}
  * Can be a representation of a rulset, if each of members is arrow.
  * So,
  *   { a -> b, c -> d }, so have hight-level optimized operations for this,
  */
trait SetTerm extends MultiTerm {


  def applyOne(term:PointTerm): MultiTerm

  def applyAll(term:PointTerm): MultiTerm

  def apply(term: PointTerm): MultiTerm =
    applyOne(term)


  /**
    * select part which comply pattern
    */
  def selectOne(pattern: TermInContext): TermInContext

  def selectAll(pattern: TermInContext): Seq[TermInContext]

  def mapReduce[A](map: PointTerm => A)(reduce:(A, A) => A)(zero: =>A):A

  def members(): Seq[PointTerm]

  override def subst(context: MultiTerm): MultiTerm = {
    mapReduce(_.subst(context))(_ or _)(EmptyTerm)
  }

  override def unify(arg: TermInContext): TermInContext = {
    mapReduce(_ unify arg)(_ or _)(TermInContext.empty)
  }

  override lazy val resolved = mapReduce(_.resolved)(_ or _)(EmptyTerm)


}


object SetTerm
{

  // Default constructor
  def create(subterms: PointTerm*):MultiTerm =
    new SeqSetTerm(subterms)

  def fromSeq(subterms: Seq[PointTerm]):MultiTerm =
    new SeqSetTerm(subterms)

  def fromMap(map:Map[Name,MultiTerm]):SetTerm = {
    val seq = map.foldLeft(IndexedSeq[PointTerm]()){ case (s,(n,v)) =>
      val a = ArrowTerm(n,v)
      s :+ a
    }
    new SeqSetTerm(seq)
  }

}


// TODO: implement.
//  emptyseq == EmptyTerm
// This is the most simply unoptimized form, which will be changed later.
class SeqSetTerm(inSeq: Seq[PointTerm]) extends SetTerm
{

  val seq = inSeq.toIndexedSeq

  override def applyOne(term:PointTerm): MultiTerm = {
    var r: MultiTerm = EmptyTerm
    // TODO: while find, applyAll ?
    seq.find{ t =>
      t match {
        case IsArrowTerm(a) =>
          val u = a.left.unify(term ^^ EmptyTerm)
          if (u.term.isExists()) {
            r=a.right.subst(u.context)
            true
          } else {
            false
          }
        case _ => false
      }
    }
    r
  }

  override def applyAll(term: PointTerm): MultiTerm = {
    val s0: MultiTerm = EmptyTerm
    seq.foldLeft(s0) { (s, t) =>
      if (s.isContradiction()) {
        s
      } else {
        t match {
          case IsArrowTerm(a) =>
            val u = a.left.unify(term ^^ EmptyTerm)
            if (u.term.isEmpty()) {
              // skip
              s
            } else if (u.term.isContradiction()) {
              u.term
            } else {
              s or a.right.subst(u.context)
            }
          case _ => s
        }
      }
    }
  }


  override def selectOne(pattern: TermInContext): TermInContext = {
    var r: TermInContext = TermInContext.empty
    // TODO: think about findAll semantics ?
    seq.find{ t =>
      r = t unify pattern
      !r.term.isEmpty()
    } match {
      case Some(_) => r
      case None => EmptyTerm ^^ pattern.context
    }
  }


  override def selectAll(pattern: TermInContext): Seq[TermInContext] = {
     val s0: Seq[TermInContext] = Seq.empty
     seq.foldLeft(s0){ (s,e) =>
       val r = e unify pattern
       if (r.term.isEmpty())
         s
       else
         s :+ r
     }
  }



  override def mapReduce[A](mapf: PointTerm => A)(reduce: (A, A) => A)(zero: =>A):A = {
    val s0 = zero
    seq.foldLeft(s0){ (s,e) =>
      reduce(s,mapf(e))
    }
  }


  override def or(otherTerm: MultiTerm): MultiTerm = {
    otherTerm.kind match {
      case k:PointTermKind =>
        addOrPoint(k.pointTerm(otherTerm))
      case k:EmptyTermKind =>
        this
      case k:ContradictionTermKind =>
        otherTerm
      case k:StarTermKind =>
        // !!!TODO:  add to context or.
        otherTerm
      case k: SetTermKind =>
        val otherSet = k.set(otherTerm)
        val s0: MultiTerm = this
        otherSet.members().foldLeft(s0){ (s,e) =>
           s match {
             case IsSetTerm(x) => x or e
             case _ => s
           }
        }
      case k: OrElseTermKind =>
        k.cast(otherTerm).map(p => this or p)
    }
  }


  def addOrPoint(otherTerm: PointTerm): MultiTerm = {
    val selected = selectOne(otherTerm ^^ EmptyTerm)
    if (selected.term.isEmpty()) {
      new SeqSetTerm(seq :+ otherTerm)
    } else {
      // !!!TODO: create normal structured term for error
      ContradictionTerm.contradiction(selected.term)
    }
  }


  override def members(): Seq[PointTerm] = seq

  override def kind: MultiTermKind = SeqSetTermKind

  def compatibleOr(other: MultiTerm): MultiTerm = {
    other.kind match {
      case k:PointTermKind =>
         val pointOther = k.pointTerm(other)
         pointOther match {
           case IsArrowTerm(a) =>
             var matched: TermInContext = TermInContext.empty
             var matchedRight: MultiTerm = EmptyTerm
             var originLeft: MultiTerm = EmptyTerm
             val index = seq.indexWhere{ p =>
               p match {
                 case IsArrowTerm(ap) =>
                   val m = ap.left unify( pointOther ^^ EmptyTerm)
                   if (!m.term.isEmpty()) {
                     matched = m
                     originLeft = ap.left
                     matchedRight = ap.right.subst(m.context)
                     true
                   }else false
                 case _ => false
               }
             }
             if (index == -1) {
               SetTerm.fromSeq(seq :+ a)
             } else {
               val newRight = matchedRight compatibleOr a.right
               if (newRight.isExists()) {
                 val newArrow = ArrowTerm(matched.term, newRight)
                 if (matched.context.isEmpty() ){
                   val newMembers = members().updated(index,newArrow)
                   SetTerm.fromSeq(newMembers)
                 } else {
                   // old - as old, new - to new ?
                   // add new at first, then old.
                   val newArrows = Seq(newArrow,seq(index))
                   SetTerm.fromSeq(members().patch(index,newArrows,2))
                 }
               } else {
                 // incompatible
                 EmptyTerm
               }
             }
           case _ =>
             mapReduce(_ and pointOther)(_ or _)(EmptyTerm)
         }
      case k:EmptyTermKind => EmptyTerm
      case k:StarTermKind => val ctx = k.star(other).context
        val checkExpr = ctx.resolve(KernelNames.checkName)
        import KernelLanguage._
        val checkWithArg = Apply(checkExpr,this)
        if (evalCondition(checkWithArg^^EmptyTerm)) {
          other
        } else {
          EmptyTerm
        }
      case k:ContradictionTermKind => other
      case k:SetTermKind => val otherSet = k.set(other)
        val s0:MultiTerm = this
        otherSet.members().foldLeft(s0){ (s,e) =>
          s compatibleOr e
        }
    }
  }


  override def resolve(term: MultiTerm): MultiTerm = {
    seq.view.map(_.resolve(term)).find(!_.isEmpty()).getOrElse(EmptyTerm)
  }


}

object SeqSetTermKind extends SetTermKind
{

}


object IsSetTerm
{

  def unapply(arg: MultiTerm): FastRefOption[SetTerm] = {
    arg.kind match {
      case k:SetTermKind => new FastRefOption[SetTerm](k.set(arg))
      case _ => new FastRefOption[SetTerm](null)
    }
  }

}

  //TODO: Implement
// -- empty-arrows == EmptyTerm
abstract class MapArrowsSetTerm(arrows: Map[PointTerm,MultiTerm]) extends SetTerm

// TODO: Implement
abstract class NetworkSetTerm(byArity: Map[Int,MapArrowsSetTerm]) extends SetTerm
