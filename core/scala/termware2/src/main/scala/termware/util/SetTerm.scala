package termware.util

import termware.{MultiTerm, MultiTermOps, PointTerm, SetTerm}

trait SetTermOps extends MultiTermOps {

  this: SetTerm =>

  def mapReduce[A](map: MultiTerm => A)(reduce:(A, A) => A)(zero: =>A):A

  def members(): Seq[MultiTerm]


}
