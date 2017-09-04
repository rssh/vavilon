package termware


import scala.language.higherKinds

trait TermLanguage {

   def name: AtomTerm

   def typer: TermTransformer

   def interpreter: TermTransformer

}
