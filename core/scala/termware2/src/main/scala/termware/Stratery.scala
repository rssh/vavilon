package termware

import cats.effect.IO

trait Stratery {


   def transform(term: MultiTerm)(f: MultiTerm => MultiTerm): IO[MultiTerm]

}
