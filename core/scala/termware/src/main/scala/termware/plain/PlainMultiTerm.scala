package termware.plain

import termware._

sealed trait MergePolicy
object MergePolicy
{
  case object LeftFirst extends MergePolicy
  case object RightFirst extends MergePolicy
  case object Unificate  extends MergePolicy
  case object Disallow  extends MergePolicy
}

