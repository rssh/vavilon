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

  override def pointUnify(ptk: PointTermKind, term: PointTerm): MultiTerm = {
    term.kind match {
      case k:StructuredTermKind =>
        structuredUnify(k,k.cast(term))
      case _ => EmptyTerm
    }
  }

  def structuredUnify(ptk: PointTermKind, otherTerm: StructuredTerm): MultiTerm = {
    if (arity == otherTerm.arity) {
      val unifiedName = nameTerm.addExternalContext(externalContext()).unify(otherTerm.nameTerm)
      if (unifiedName.isExists()) {
        unifiedName.kind match {
          case x:AtomTermKind =>
            val newName = x.cast(x.pointTerm(unifiedName))
            val newSubterms = NameIndexed.empty[MultiTerm]
            val s0:MultiTerm = StructuredTerm.create(newName,newSubterms).setExternalContext(unifiedName.externalContext())
            namedSubterms().foldWhile(s0)(_.isExists()){ case (s,(n,v)) =>
              otherTerm.get(n) match {
                case None => s
                case Some(otherValue) =>
                  s match {
                    case IsStructuredTerm(ss) =>
                      v.addExternalContext(s) unify otherValue
                    case _ => s
                  }
              }
            }
          case otherKind =>
            //TODO: add error to context.
            EmptyTerm
        }
      } else {
        EmptyTerm
        // TermInContext(EmptyTerm,KernelLanguage.contextWithFailure(u.context,"name mismatch"))
      }
    } else {
      EmptyTerm
      //TermInContext(EmptyTerm,KernelLanguage.contextWithFailure(u.context,"arity mismatch"))
    }
  }

  def updated(name:AtomName, value: MultiTerm): Self

  override def dropExternalContext(): StructuredTerm with NoExternalContext


}

object StructuredTerm extends StructuredTermKind
{

  override def cast(x: PointTerm): StructuredTerm = x.asInstanceOf[StructuredTerm]

  def create(nameTerm: AtomTerm, indexes: NameIndexed[MultiTerm]): StructuredTerm =
    PlainStructuredTerm(nameTerm,indexes)


}

case class PlainStructuredTerm(override val nameTerm: AtomTerm,
                               indexes:NameIndexed[MultiTerm]) extends StructuredTerm with PointTermNoExternalContext
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

  override def updated(name: AtomName, value: MultiTerm): PlainStructuredTerm = {
    // TODO:  check that value satisficy context.
    PlainStructuredTerm(name,indexes.updated(name,value))
  }

  override def mapSubterms(f: MultiTerm => MultiTerm): PlainStructuredTerm = {
    // TODO: check that resilt is correct
    PlainStructuredTerm(nameTerm,indexes.map(f))
  }

  override def pushInternalContext(context: MultiTerm): MultiTerm = {
    val newNameM = nameTerm.pushInternalContext(context)
    newNameM.kind match {
      case k: AtomTermKind => val newName = k.atomTerm(k.pointTerm(newNameM))
                              PlainStructuredTerm(newName,indexes)
      case other => // impossible.
            // TODO: restore contradiction term ?
            EmptyTerm
    }

  }

  override def dropExternalContext(): StructuredTerm with NoExternalContext = {
    this
  }

}

class StructuredTermInExternalContext(base: PlainStructuredTerm, externContext: MultiTerm)
   extends TermInExternalContext(base,externContext) with StructuredTerm {

  override type Self = StructuredTermInExternalContext


  override def nameTerm: AtomTerm = base.nameTerm

  override def get(name: AtomName): Option[MultiTerm] = base.get(name).map(TermInExternalContext(_,externContext))

  override def get(i: Int): Option[MultiTerm] = base.get(i).map(TermInExternalContext(_,externContext))

  override def names(): IndexedSeq[AtomName] = base.names()

  override def indexedSubterms(): IndexedSeq[MultiTerm] = base.indexedSubterms().map(TermInExternalContext(_,externContext))

  override def namedSubterms(): NameIndexed[MultiTerm] = base.namedSubterms().map(TermInExternalContext(_,externContext))

  override def newNamedSubterms(newIndexes: NameIndexed[MultiTerm]): Self = {
    new StructuredTermInExternalContext(base.newNamedSubterms(newIndexes), externContext)
  }

  override def mapSubterms(f: MultiTerm => MultiTerm): Self = {
    new StructuredTermInExternalContext(base.mapSubterms(f),externContext)
  }

  override def updated(name: AtomName, value: MultiTerm): Self = {
    new StructuredTermInExternalContext(base.updated(name,value),externContext)
  }

  override def dropExternalContext(): StructuredTerm with NoExternalContext = base

  override def name: Name = base.name

  override def arity: Int = base.arity
}

object StructuredTermInExternalContext
{

  def apply(base: StructuredTerm with NoExternalContext, externContext: MultiTerm): MultiTerm = {
    if (externContext.isEmpty()) {
      EmptyTerm
    } else if (externContext.isStar()) {
      base
    } else if (base.isInstanceOf[PlainStructuredTerm]) {
      new StructuredTermInExternalContext(base.asInstanceOf[PlainStructuredTerm], externContext)
    } else {
      val nBase = new PlainStructuredTerm(base.nameTerm, base.namedSubterms())
      new StructuredTermInExternalContext(nBase, externContext)
    }
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