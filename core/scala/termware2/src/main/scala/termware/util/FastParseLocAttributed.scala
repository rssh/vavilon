package termware.util

import fastparse.all._
import fastparse.core
import fastparse.core.{Mutable, ParseCtx, Parser}

object FastParseLocAttributed {

  case class SourceLocation(
      file:String,
      line:Int,
      row:Int
  )

  case class InSourceLocation(startLine:Int, lines:Int, lastEOLIndex:Int, startIndex:Int, endIndex:Int)

  object InSourceLocation {
    val ZERO = InSourceLocation(0,0,0,0,0)
  }


  case class InSource[+T](value:T,location:InSourceLocation)
  {
    def map[S](f:T=>S):InSource[S] = InSource(f(value),location)
  }

  val laStart: Parser[InSource[Unit],Char,String] = fastparse.parsers.Terminals.PassWith(
    InSource[Unit]((),InSourceLocation.ZERO))




  def laCharPred(predicate:Char=>Boolean):Parser[InSource[Char],Char,String]=new Parser[InSource[Char],Char,String]() {
    override def parseRec(cfg: ParseCtx[Char, String], index: Int): core.Mutable[InSource[Char], Char, String] = {
      if (! cfg.input.isReachable(index)) fail(cfg.failure,index)
      else {
        val ch = cfg.input(index)
        if (predicate(ch)) {
          val lines = if (ch == '\n') 1 else 0
          val lineEndIndex = if (ch == '\n') index else -1
          val retval = InSource(ch, InSourceLocation(0, lines, lineEndIndex, index, index))
          success(cfg.success, retval, index + 1, Set.empty, false)
        } else {
          fail(cfg.failure, index)
        }
      }
    }
  }

  def laCharIn(chars:Set[Char]):Parser[InSource[Char],Char,String] = laCharPred(chars.contains)

  def laCharsWhile(predicate:Char=>Boolean):Parser[InSource[String],Char,String] = new Parser[InSource[String],Char,String] {
    override def parseRec(cfg: ParseCtx[Char, String], index: Int): Mutable[InSource[String], Char, String] = {
      var i = index
      val sb = new StringBuilder
      var nLines = 0
      var lastEolIndex = -1
      var quit = false
      while(!quit) {
        if (!cfg.input.isReachable(i)) {
          quit = true
        } else {
          val ch = cfg.input(i)
          if (!predicate(ch)) {
            quit = true
          } else {
            sb.append(ch)
            i += 1
            if (ch == '\n') {
              nLines += 1
              lastEolIndex = i
            }
          }
        }
      }
      if (sb.isEmpty) {
        fail(cfg.failure,i)
      } else {
        val  location = InSourceLocation(-1,nLines,lastEolIndex,index,i)
        success(cfg.success,InSource(sb.toString,location),i,Set(),false)
      }
    }
  }

  val laWhitespace:Parser[InSource[Unit],Char,String] = laCharsWhile(_.isWhitespace).map(_.map(_=>()))

  def laLiteral(value:String):Parser[InSource[String],Char,String] = new Parser[InSource[String],Char,String] {
    override def parseRec(cfg: ParseCtx[Char, String], index: Int): Mutable[InSource[String], Char, String] = {
      var i=index
      var li=0
      var lastEolIndex = -1
      var quit = false
      var failure = false
      var nLines = 0
      while(!quit && !failure) {
        if (!cfg.input.isReachable(i)) {
          failure = true
        }
        val ch=cfg.input(i)
        if (ch==value.charAt(li)) {
          li+=1
          i+=1
          quit=(li==value.size)
        } else {
          failure=true
        }
      }
      if (failure) {
        fail(cfg.failure,i,Set(),false)
      } else {
        val  location = InSourceLocation(-1,nLines,lastEolIndex,index,i)
        success(cfg.success,InSource(value,location),i,Set(),false)
      }
    }

  }


  def combine2[A,B,C](f:(A,B)=>C):(InSource[A],InSource[B])=>InSource[C] = {
    (x,y) =>
      InSource(f(x.value,y.value),combineLoc2(x.location,y.location))
  }

  def combineLoc2(x:InSourceLocation, y:InSourceLocation): InSourceLocation = {
    val lastEOLIndex = if (y.lastEOLIndex != -1) {
      y.lastEOLIndex
    } else {
      x.lastEOLIndex
    }
    InSourceLocation(x.startLine,x.lines+y.lines,lastEOLIndex,x.startIndex,y.endIndex)
  }

  def combine3[A,B,C,D](f:(A,B,C)=>D):((InSource[A],InSource[B],InSource[C])=>InSource[D])=
   { (x,y,z) =>
     InSource(f(x.value,y.value,z.value),combineLoc3(x.location,y.location,z.location))
   }

  def combineLoc3(x:InSourceLocation, y:InSourceLocation, z:InSourceLocation): InSourceLocation = {
    combineLoc2(combineLoc2(x,y),z)
  }

  def combine4[A,B,C,D,E](f:(A,B,C,D)=>E):((InSource[A],InSource[B],InSource[C],InSource[D])=>InSource[E])=
  { (x,y,z,v) =>
    InSource(f(x.value,y.value,z.value,v.value),combineLoc4(x.location,y.location,z.location,v.location))
  }

  def combineLoc4(x:InSourceLocation, y:InSourceLocation, z:InSourceLocation, v:InSourceLocation): InSourceLocation = {
    combineLoc2(combineLoc2(combineLoc2(x,y),z),v)
  }



  def combineSeq[A,B](f:Seq[A]=>B):(Seq[InSource[A]]=>InSource[B]) = { (seq) =>
      InSource(f(seq map (_.value)),combineLocSeq(seq map (_.location)))
  }

  def combineLocSeq(seq:Seq[InSourceLocation]):InSourceLocation =
  {
    if (seq.isEmpty) {
      InSourceLocation(0,0,-1,0,0)
    }else{
      seq.tail.foldLeft(seq.head)(combineLoc2)
    }
  }

  def laWhitespaceSeq[T,S](frs:Parser[InSource[T],Char,String],
                           snd:Parser[InSource[S],Char,String]): Parser[InSource[(T,S)],Char,String] = {
    (frs ~ laWhitespace ~ snd).map{case (x,y,z) => combine3[T,Unit,S,(T,S)]((x,y,z)=>(x,z))(x,y,z)}
  }

  implicit class InSourceParserOps[T](val p:Parser[InSource[T],Char,String]) extends AnyVal
  {

    def pmap[S](f:T=>S):Parser[InSource[S],Char,String] = {
      p.map{ case r => r.map(f)  }
    }

    def ~~[S](s:Parser[InSource[S],Char,String]) =
      laWhitespaceSeq[T,S](p,s)


  }

}
