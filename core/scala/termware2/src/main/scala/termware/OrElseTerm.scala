package termware

trait OrElseTerm extends MultiTerm {


  /**
    * Find term which satisficy predicate or empty term if not found.
    * @param p
    * @return
    */
  def firstNotEmpty(p: MultiTerm => MultiTerm): MultiTerm = {
    firstMapped(p)(! _.isEmpty() )(EmptyTerm)
  }


  def firstMapped[A](f:MultiTerm => A)(p:A => Boolean)(default: =>A): A


  /**
    * Create orElseterm which each term is mapped.
    * @param f
    * @return
    */
  def map(f:MultiTerm => MultiTerm): MultiTerm


  def orElse(t:MultiTerm): MultiTerm

}
