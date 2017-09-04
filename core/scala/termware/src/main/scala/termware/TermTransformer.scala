package termware

import cats.effect.Effect

import scala.language.higherKinds

trait TermTransformer {


    def transform[Eff[_]](x:Eff[MultiTerm])(implicit effect:Effect[Eff]): Eff[MultiTerm]


}
