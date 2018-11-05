package termware.util

import termware.MultiTermKind

class UnapplyCastWrapper[K <: MultiTermKind](val value:K) extends AnyVal {

  def unapply(arg: MultiTermKind): FastRefOption[K#Out] =
    new FastRefOption(arg.cast(value.asInstanceOf[arg.In]).asInstanceOf[K#Out])

}
