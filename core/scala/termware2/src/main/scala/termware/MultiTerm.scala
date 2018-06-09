package termware

import cats.effect.IO

trait MultiTerm
{
  def kind:MultiTermKind

  def resolved(): MultiTerm

  def resolve(term:MultiTerm): MultiTerm

  def apply(term:PointTerm): MultiTerm

}

