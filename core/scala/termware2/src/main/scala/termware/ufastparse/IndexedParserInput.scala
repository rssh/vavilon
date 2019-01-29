package termware.ufastparse

import java.io.{LineNumberReader, Reader, StringReader}

import fastparse.ParserInput
import termware.util.SourceLocation

import scala.collection.immutable.TreeMap

trait IndexedParserInput extends ParserInput {

  def fname():String

  def markTo(index: Int):SourceLocation

  def reader: java.io.Reader

}

class StringBasedIndexedParserInput(s: String, override val fname: String) extends IndexedParserInput {

  var indexToLine: TreeMap[Long,Long] = TreeMap.empty

  override def markTo(index: Int): SourceLocation = {
    val before = indexToLine.to(index)
    val (lastIndex,lastLine) = before.lastOption.getOrElse((0L,index.toLong))
    SourceLocation(fname,lastLine.toInt, (index - lastIndex).toInt)
  }

  override def reader: Reader = new IndexedWrappedReader(new StringReader(s),
    (line,index)=> indexToLine = indexToLine.updated(line,index) )

  override def apply(index: Int): Char = {
    s.charAt(index)
  }

  override def dropBuffer(index: Int): Unit = {}

  override def slice(from: Int, until: Int): String = {
    s.substring(from,until-1)
  }

  override def length: Int = s.length

  override def innerLength: Int = s.length

  override def isReachable(index: Int): Boolean = {
    index < s.length
  }

  override def checkTraceable(): Unit = {

  }

  override def prettyIndex(index: Int): String = {
    s"${fname}:${index}"
  }
}



object IndexedParserInput {

  def fromString(s:String, file: String) =
    new StringBasedIndexedParserInput(s,file)

  def fromFile(path: String) = {
    fromString(scala.io.Source.fromFile(path).mkString,path)
  }

}

