package termware.util

import termware.{MultiTerm, PointTerm}

trait SetTerm extends MultiTerm {

  def mapReduce[A](map: MultiTerm => A)(reduce:(A, A) => A)(zero: =>A):A

  def members(): Seq[MultiTerm]


}
