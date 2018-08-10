package parser

class TokenMgrError extends Error {
  var errorCode: Int = _

  override def getMessage: String = super.getMessage

  def this(message: String, reason: Int) = {
    this
    errorCode = reason
  }

  def this(EOFSeen: Boolean,
           lexState: Int,
           errorLine: Int,
           errorColumn: Int,
           errorAfter: String,
           curChar: Char) = {
    this
    TokenMgrError.LexicalError(EOFSeen,
      lexState,
      errorLine,
      errorColumn,
      errorAfter,
      curChar)
  }
}

object TokenMgrError {
  val LEXICAL_ERROR = 0
  val STATIC_LEXER_ERROR = 1
  val INVALID_LEXICAL_STATE = 2
  val LOOP_DETECTED = 3

  def addEscapes(str: String): String = {
    val retval: StringBuilder = StringBuilder()
    var ch: Char = ' '
    str.foreach {
      x =>
        x match {
          case 0 =>
          case '\b' => retval.append("\\b")
          case '\t' => retval.append("\\t")
          case '\n' => retval.append("\\n")
          case '\f' => retval.append("\\f")
          case '\r' => retval.append("\\r")
          case '\"' => retval.append("\\\"")
          case '\'' => retval.append("\\\'")
          case '\\' => retval.append("\\\\")
          case _ =>
            ch = x
            if (ch < 0x20 || ch > 0x7e) {
              val s = s"0000${Integer.toHexString(ch)}"
              retval.append(s"\\u${s.substring(s.length - 4, s.length)}")
            } else
              retval.append(ch)
        }
    }
    retval.toString
  }

  def LexicalError(EOFSeen: Boolean,
                   lexState: Int,
                   errorLine: Int,
                   errorColumn: Int,
                   errorAfter: String,
                   curChar: Char): String = {
    s"""Lexical error at line $errorLine, column $errorColumn
       |.  Encountered:
       | ${if (EOFSeen) "<EOF" else addEscapes(curChar.toString)}
       |   ( ${curChar.toInt}), after : ${addEscapes(errorAfter)}
     """.stripMargin
  }


}
