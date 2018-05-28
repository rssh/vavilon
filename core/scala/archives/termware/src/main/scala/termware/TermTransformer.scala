package termware

import cats.effect.Effect

import scala.language.higherKinds

trait TermTransformer {


    def transform(x: MultiTerm): MultiTerm


}
