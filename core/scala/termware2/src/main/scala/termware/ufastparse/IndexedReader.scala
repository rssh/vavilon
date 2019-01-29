package termware.ufastparse

import java.io.Reader

abstract class IndexedReader extends Reader {

  def lineNumber: Long

  def columnNumber: Int

  def index: Long

}


class IndexedWrappedReader(in: Reader, indexCallback: (Long,Long) => Unit) extends IndexedReader {

  var lineNumber: Long = 0

  var columnNumber: Int = 0

  var index: Long = 0

  override def read(): Int = {
    val ch = super.read();
    if (ch=='\n') {
      lineNumber += 1
      columnNumber = 0
      indexCallback(lineNumber, index+1)
    } else {
      columnNumber += 1
    }
    index += 1
    ch
  }

  override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    val r = in.read(cbuf,off,len)
    if (r > 0) {
      var i = 0;
      while(i < r) {
        if (i=='\n') {
          lineNumber += 1
          columnNumber = 0
          indexCallback(lineNumber, index+1)
        } else {
          columnNumber += 1
        }
        index += 1
        i += 1
      }
    }
    r
  }

  override def skip(n: Long): Long = {
    var rest = n
    val maxSkipBufferSize = 1024*64
    val skipBufferSize: Int = if (n < maxSkipBufferSize.toLong) n.toInt else maxSkipBufferSize
    val skipBuffer = new Array[Char](skipBufferSize)
    var sr = 0L
    var r = 0
    while(rest > 0 && r >= 0) {
      r = read(skipBuffer,0,if (rest <= skipBufferSize) rest.toInt else skipBufferSize )
      if (r > 0) {
        rest -= r
      } else {
        sr += r
      }
    }
    sr
  }

  override def close(): Unit = {
    in.close()
  }
}
