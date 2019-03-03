package termware

import termware.util.{SetTermOps}

/**
  * This is one non-contradiction term.
  * EmptyCoset is star.
  */
trait AndSetTermOps extends SetTermOps {

  this: AndSetTerm =>

  override def kind: MultiTermKind = AndSetTerm.Kind

  override def dropExternalContext(): AndSetTerm with NoExternalContext

}

object AndSetTermOps  {

  def createPoints(subterms:PointTerm*): MultiTerm =
    new impl.SeqAndSetTerm(subterms)

}




