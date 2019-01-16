package termware

import termware.util.{FastRefOption, SeqSetTerm, SetTerm}

/**
  * This is one non-contradiction term.
  * EmptyCoset is star.
  */
trait AndSetTerm extends SetTerm {

  override def kind: MultiTermKind = AndSetTerm.Kind

  override def dropExternalContext(): AndSetTerm with NoExternalContext

}



object AndSetTerm  {

  def createPoints(subterms:PointTerm*): MultiTerm =
    new impl.SeqAndSetTerm(subterms)

  object Kind extends AndSetTermKind


}

