package termware


/**
  * Sequence term
  * seq(a_1....a_n)
  */
trait SequenceTerm extends PointTerm {

  thisSequenceTerm =>

  type Self <: SequenceTerm

  def subterm(i:Int): MultiTerm

  def subterms(): Vector[MultiTerm]

  def append(x:MultiTerm): SequenceTerm

  override def pointKind: PointKind = PointKind.Sequence(this)

  override def uncontext: SequenceTerm with EmptyContext

  override def name: Name = SeqName

  /**
    * context of headTerm can looks like:
    * <pre>
    * constraints -> {
    *   element -> x -> "logical-expression"
    *   length -> l -> logical-expression
    * }
    * </pre>
    * @return term with name sequence and context with constraints (if we have one).
    */
  def nameTerm: PointTerm

  override def pointUnify(other: PointTerm): MultiTerm =
     other.pointKind match {
       case PointKind.Sequence(otherSeq) =>
         if (otherSeq.arity != arity) {
           EmptyTerm
         } else {
           // { element:
           // TODO: implement select
           import KernelLanguage._
           val otherChecks: MultiTerm = other.nameTerm.resolve(KernelNames.checkNameTerm)
           val otherElementChecks = otherChecks.and(KernelNames.elementNameTerm ~> DefaultStarTerm)
           val nsubs = subterms().zip(otherSeq.subterms()).map{ x =>
             // TODO: find example, where we need to check element.
             //   in theorty.  x:A, y:B => unify(x,y):A&B, but need to proove.
               (x._1 unify x._2)
           }
           if (nsubs.forall(_.isExist)) {
             (nameTerm and other.nameTerm).multiKind match {
               case MultiKind.Point(pt) =>
                     new PlainSequenceTerm(pt , nsubs)
               case MultiKind.Contradiction(ct) => ct
               case MultiKind.Empty(e) => e
               case x =>  ContextContradictionTerm.withMessages("operation" -> "head unification")
             }
           } else {
             ContextContradictionTerm.withMessages("operation" -> "unification")
           }
         }
       case _ => EmptyTerm
     }

  def newSubterms(value:Vector[MultiTerm]): SequenceTerm



}


case class PlainSequenceTerm(override val nameTerm: PointTerm = KernelNames.seqNameTerm,  values:Vector[MultiTerm]) extends SequenceTerm with EmptyContext
{

  override type Self = PlainSequenceTerm

  override def subterm(i: Int): MultiTerm = {
    if (i < values.size && i>0) {
      values(i)
    }else{
      EmptyTerm
    }
  }

  override def subterms(): Vector[MultiTerm] = values

  override def arity: Int = values.size

  // are we need uncontext at all ???
  override def uncontext: PlainSequenceTerm = this

  override def in(ctx: MultiTerm): MultiTerm = {
    // TODO:  check - are we want mark head ?
    val headContext = ctx.resolve(KernelNames.nameTerm)

    val constraints = nameTerm.in(headContext).resolve(KernelNames.constraintsNameTerm)

    val elementsConstraints = constraints.resolve(KernelNames.elementNameTerm)

    val lengthConstraints = constraints.resolve(KernelNames.lengthNameTerm)

    lengthConstraints.check(values.size)

    values.forall(x => elementsConstraints.check(x))


    nameTerm.in(headContext).multiKind match {
      case MultiKind.Point(nextName) =>
        val nextValues = for((v,i) <- values.zipWithIndex) yield {
          val head = headContext.subterm(i)
          v in (head orElse ctx)
        }
        //TODO: run check ?
        PlainSequenceTerm(nextName,nextValues)
      case MultiKind.Contradiction(ct) => ct
      case MultiKind.Empty(e) => e
      case MultiKind.SeqOr(seqOr) => ???
      case MultiKind.Set(set) => ???
      case MultiKind.Star(st) =>

    }



  }

  override def orPoint(other: PointTerm): MultiTerm =
     other.pointKind match {
       case PointKind.Sequence(otherSeq) =>
          if (termEq(otherSeq)) {
            this
          } else {
            SetTerm.create(this,otherSeq)
          }
       case _ => SetTerm.create(this,other)
     }

  override def andPoint(other: PointTerm): MultiTerm =
     other.pointKind match {
       case PointKind.Sequence(otherSeq) =>
         if (this.arity == otherSeq.arity) {
           val v = (values zip otherSeq.subterms()).map{
             case (x,y) => x and y
           }
           v.find(_.isEmpty) match {
             case Some(_) => EmptyTerm
             case None => new PlainSequenceTerm(v)
           }
         }else{
           EmptyTerm
         }
       case _ => EmptyTerm
     }

  override def apply(other: MultiTerm): MultiTerm = EmptyTerm

  override def subst(x: MultiTerm): MultiTerm = {
    new PlainSequenceTerm(values.map(_.subst(x)))
  }

  override def append(x: MultiTerm): SequenceTerm = {
    new PlainSequenceTerm(values :+ x)
  }

  override def newSubterms(value: Vector[MultiTerm]): PlainSequenceTerm =
    new PlainSequenceTerm(value)

}

object PlainSequenceTerm
{

  def create(values: MultiTerm*): PlainSequenceTerm =
  {
    new PlainSequenceTerm(KernelNames.seqNameTerm, values.toVector)
  }

  val empty = new PlainSequenceTerm(KernelNames.seqNameTerm, Vector())

}

