package parser

import java.io.PrintStream

class ParserTokenManager extends ParserConstants{
  var debugStream: PrintStream = System.out

  private[parser] var curLexState = 0
  private[parser] var defaultLexState = 0
  private[parser] var newStateCnt = 0
  private[parser] var round = 0
  private[parser] var matchedPos = 0
  private[parser] var matchedKind = 0


  protected var input_stream:SimpleCharStream = _
  protected var curChar:Char = _

  private def stopAtPos(pos:Int,kind:Int): Int ={
    var p = pos
    matchedKind = kind
    matchedPos = pos
    p += 1
    p
  }

  def getNextToken():Token = {
    var specialToken:Token = null
    var matchedToken:Token = null
    var curPos = 0
    var failed = true

    while(failed){
      curChar = input_stream
    }

  }
}
