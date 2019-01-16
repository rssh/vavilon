package termware.util

import termware.StructuredTerm

import termware.DSL._

case class SourceLocation(path:String, line: Int, column: Int)
{

  def asTerm(): StructuredTerm = {
    'SourceLocation('path -> path, 'line -> line, 'column -> column)
  }

}

