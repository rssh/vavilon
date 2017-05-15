package termware.operations

import termware.{MergePolicy, MultiTerm}

object Merge extends Function3[MultiTerm,MultiTerm,MergePolicy,MultiTerm]
{

  override def apply(x: MultiTerm, y: MultiTerm, mp:MergePolicy): MultiTerm = ???


}
