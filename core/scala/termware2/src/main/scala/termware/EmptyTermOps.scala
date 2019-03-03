package termware

import termware.util.{FastRefOption}


trait EmptyTermOps extends MultiTermOps with OrSetTermOps with AndSetTermOps with NoExternalContext
{

  this: EmptyTerm.type =>

  override def kind: MultiTermKind = EmptyTermKind

  final val Kind = EmptyTermKind

  override def resolve(term:MultiTerm): MultiTerm = EmptyTerm

  //TODO: rethink
  override def termApply(term: PointTerm): MultiTerm = EmptyTerm

  def context(): MultiTerm = this

  override def subst(context: MultiTerm): MultiTerm = this

  override def unify(x: MultiTerm): MultiTerm = this

  override def or(x: MultiTerm): MultiTerm = x

  override def and(x: MultiTerm): MultiTerm = this

  override def mapReduce[A](map: MultiTerm => A)(reduce: (A, A) => A)(zero: =>A) = zero

  override def members(): Seq[PointTerm] = Seq.empty

  // adding context to empty term is useless.
  override def pushInternalContext(context: MultiTerm): MultiTerm = this

  override def dropExternalContext(): EmptyTerm.type with NoExternalContext = this

}

object IsEmptyTerm
{

  def unapply(x:MultiTerm):FastRefOption[EmptyTerm.type]={
    if (x==EmptyTerm) FastRefOption(EmptyTerm) else FastRefOption.empty
  }

}
