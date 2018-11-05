package termware

import termware.util.{FastRefOption, NameIndexed}

trait StructuredTerm extends PointTerm  with ContextCarrierTerm {

  type Self <: StructuredTerm

  override def kind: StructuredTermKind = StructuredTerm

  def nameTerm: AtomTerm


  def get(name: AtomName): Option[MultiTerm]

  /**
    * @param i : 0 < i < arity
    * @return
    */
  def get(i:Int):Option[MultiTerm]

  def names(): IndexedSeq[AtomName]

  def indexedSubterms(): IndexedSeq[MultiTerm]

  def namedSubterms(): NameIndexed[MultiTerm]

  def newNamedSubterms(newIndexes: NameIndexed[MultiTerm]): Self

  def mapSubterms(f:MultiTerm => MultiTerm): Self

  override def subst(context: MultiTerm): MultiTerm = {
    // TODO: think about mapping head.
    // TODO: wrap in IO
    mapSubterms(_.subst(context))
  }

  override def context(): MultiTerm = nameTerm.context()

  override def pointUnify(ptk: PointTermKind, p: PointTermInContext): TermInContext = {
    val term = p.term
    term.kind match {
      case k:StructuredTermKind =>
        structuredUnify(k,InContext(k.cast(term),p.context))
      case _ => TermInContext(EmptyTerm,p.context)
    }
  }

  def structuredUnify(ptk: PointTermKind, u: InContext[StructuredTerm]): TermInContext = {
    val otherTerm = u.term
    if (arity == otherTerm.arity) {
      val InContext(unifiedName,nContext) = nameTerm.unify(InContext(otherTerm.nameTerm,u.context))
      if (unifiedName.isExists()) {
        unifiedName.kind match {
          case x:AtomTermKind =>
            val newName = x.cast(x.pointTerm(unifiedName))
            val newSubterms = NameIndexed.empty[MultiTerm]
            val s0:InContext[MultiTerm] = InContext(
                             StructuredTerm.create(newName,newSubterms),
                             nContext)
            namedSubterms().foldWhile(s0)(_.term.isExists()){ case (s,(n,v)) =>
              otherTerm.get(n) match {
                case None => s
                case Some(otherValue) =>
                  s.term match {
                    case IsStructuredTerm(ss) =>
                      val InContext(nextValue,nextContext) = v.unify(TermInContext(otherValue,nContext))
                      TermInContext(ss.updated(n,nextValue),nextContext)
                    case _ => s
                  }
              }
            }
          case otherKind =>
            //TODO: add error to context.
            TermInContext(EmptyTerm,nContext)
        }
      } else {
         TermInContext(EmptyTerm,KernelLanguage.contextWithFailure(u.context,"name mismatch"))
      }
    } else {
      TermInContext(EmptyTerm,KernelLanguage.contextWithFailure(u.context,"arity mismatch"))
    }
  }

  def updated(name:AtomName, value: MultiTerm): MultiTerm



}

object StructuredTerm extends StructuredTermKind
{

  override def cast(x: PointTerm): StructuredTerm = x.asInstanceOf[StructuredTerm]

  def create(nameTerm: AtomTerm, indexes: NameIndexed[MultiTerm]): StructuredTerm =
    PlainStructuredTerm(nameTerm,indexes)


}

  case class PlainStructuredTerm(override val nameTerm: AtomTerm,
                               indexes:NameIndexed[MultiTerm]) extends StructuredTerm
{
  override type Self = PlainStructuredTerm

  override def name: Name = nameTerm.name

  override def get(name: AtomName): Option[MultiTerm] = indexes.get(name)

  /**
    * @param i : 0 < i < arity
    * @return
    */
  override def get(i: Int): Option[MultiTerm] = indexes.get(i)

  override def names(): IndexedSeq[AtomName] = indexes.names

  override def indexedSubterms(): IndexedSeq[MultiTerm] = indexes.records.map(_.value)

  override def namedSubterms(): NameIndexed[MultiTerm] = indexes

  override def arity: Int = indexes.size

  override def newNamedSubterms(newIndexes: NameIndexed[MultiTerm]): Self =
    PlainStructuredTerm(nameTerm,newIndexes)

  override def updated(name: AtomName, value: MultiTerm): MultiTerm = {
    // TODO:  check that value satisficy context.
    PlainStructuredTerm(name,indexes.updated(name,value))
  }

  override def mapSubterms(f: MultiTerm => MultiTerm): PlainStructuredTerm = {
    // TODO: check that resilt is correct
    PlainStructuredTerm(nameTerm,indexes.map(f))
  }


}

object IsStructuredTerm {

  def unapply(x:MultiTerm):FastRefOption[StructuredTerm]={
    x.kind match {
      case sk: StructuredTermKind =>
        new FastRefOption(sk.structured(sk.pointTerm(x)))
      case _ =>
        FastRefOption.empty
    }
  }


}