package termware



trait ErrorTerm extends MultiTerm {


}

trait ErrorTermImpl[S <: ErrorTermImpl[S]] extends ErrorTerm with MultiTermImpl[S] {

  this: S =>

  override def name: Name = ErrorName

  override def cardinality: Int = 0

  override def multiKind: MultiKind = MultiKind.Error(this)

  override def context: MultiTerm = this


}


case class DefaultErrorTerm(message:String, optException:Option[Throwable]) extends ErrorTermImpl[DefaultErrorTerm] with EmptyContext
{
  override def updateContext(ctx: MultiTerm): MultiTerm = ???

  override def uncontext: MultiTerm with EmptyContext = this

  override def mergeAsLeftContext(other: MultiTerm): MultiTerm = ???

  override def selectAsContextPattern(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeAnd(other: MultiTerm): MultiTerm = ???

  override def mergeAsScopeOr(other: MultiTerm): MultiTerm = ???

  override def selectAsLeftPattern(other: MultiTerm): MultiTerm = ???
}



