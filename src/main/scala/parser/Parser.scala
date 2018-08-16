package parser

import java.io._

import ast.{AST, ExprNode}
import compiler.ErrorHandler
import entity.Declarations
import exception.{CompileException, SyntaxException}
import parser.Parser.JJCalls

import scala.collection.mutable.ArrayBuffer


class Parser extends ParserConstants {
  private var sourceName: String = _
  private var loader: LibraryLoader = _
  private var errorHandler: ErrorHandler = _
  private var knownTypeders: Set[String] = _


  private var trace_enabled = true
  private var trace_indent = 0

  def this(r: Reader, name: String, loader: LibraryLoader, errorHandler: ErrorHandler, debug: Boolean) {
    this
    this.sourceName = name
    this.errorHandler = errorHandler
    this.loader = loader
    this.knownTypeders = Set[String]()
    if (debug)
      enable_tracing
    else
      disable_tracing
  }

  def enable_tracing = trace_enabled = true

  def disable_tracing = trace_enabled = false


  final private class LookaheadSuccess extends Error

  def parse(): AST = {

    try {
      compilation_unit()
    } catch {
      case err: TokenMgrError =>
        throw SyntaxException(err.getMessage)
      case ex: ParseException =>
        throw SyntaxException(ex.getMessage)
      case err: LookaheadSuccess =>
        throw SyntaxException("syntax error")
    }

  }

  def getNtk() = {
    nt = token.next
    nt match {
      case null =>
        token.next = token_source.getNextToken()
        ntk = token.next.kind
      case _ =>
        ntk = nt.kind
    }
    ntk
  }

  def generateParseException(): ParseException = {
    ParseException(token, null, tokenImage)
  }

  def consume_token(kind: Int): Token = {
    var oldToken = token
    token = if (token.next != null) token.next else {
      token.next = token_source.getNextToken()
      token.next
    }
    ntk = -1
    if (token.kind == kind) {
      gen += 1
      gc += 1
      if (gc > 100) {
        gc = 0
        var index = 0
        for (i <- rtns) {
          var c = i
          while (c != null) {
            if (c.cgen < gen) c.first = null
            c = c.next
          }
          index += 1
        }
      }
      trace_token(token, "")
      return token
    }
    token = oldToken
    this.kind = kind
    throw generateParseException()
  }

  def name(): String = {
    trace_call("name")
    try {
      val t = consume_token(IDENTIFIER)
      t.image
    } catch {
      case _: ParseException => throw new Error("Missing return statement in function")
    } finally {
      trace_return("name")
    }
  }

  def expr(): ExprNode = {
    trace_call("expr")
    var lhs:ExprNode = null
    var rhs:ExprNode = null
    var expr:ExprNode = null
    var op:String = ""

  }

  def args() = {
    trace_call("args")
    try {
      var argss = ArrayBuffer[ExprNode]()
      var arg: ExprNode = null
      (if (ntk == -1) getNtk() else ntk) match {
        case x if (x == SIZEOF
          | x == IDENTIFIER
          | x == INTEGER
          | x == CHARACTER
          | x == STRING
          | (x >= 87 && x <= 90)
          | x == 46
          | x == 57
          | x == 80
          | x == 83
          | x == 84) =>
          arg = expr()
          argss.+=(arg)
          var failed = true
          while (failed) {
            (if (ntk == -1) getNtk() else ntk) match {
              case 50 =>
              case _ =>
                la1(48) = gen
                failed = false
            }
            consume_token(50)
            arg = expr()
            argss.+=(arg)
          }
        case _ =>
          la1(50) = gen
      }
      argss
    } catch {
      case _: Exception =>
        throw new Error("Missing return statement in function")
    } finally {
      trace_return("args")
    }
  }

  def import_stmt(): String = {
    trace_call("import_stmt")
    try {
      val buf: StringBuilder = StringBuilder()
      consume_token(IMPORT)
      var n: String = name()
      buf.append(n)
      var failed = true
      while (failed) {
        (if (ntk == -1) getNtk() else ntk) match {
          case 47 =>
          case _ =>
            la1(3) = gen
            failed = false
        }
        consume_token(47)
        n = name()
        buf.append(".")
        buf.append(n)
      }
      consume_token(48)
      buf.toString
      throw new Error("Missing return statement in function")
    } finally {
      trace_return("import_stmt")
    }

  }

  def import_stmts(): Declarations = {
    trace_call("import_stmts")
    try {
      var libid: String = ""
      var impdecls: Declarations = new Declarations()
      var failed = true
      while (failed) {
        (if (ntk == -1) getNtk() else ntk) match {
          case IMPORT =>
          case _ =>
            la1(2) = gen
            failed = false
        }
        libid = import_stmt()
        try {
          val decls = loader.loadLibrary(libid, errorHandler)
          if (decls != null) {
            impdecls.add(decls)
            addKnownTypedefs(decls.typedefs())
          }
        } catch {
          case e: CompileException => throw new ParseException(e.getMessage)
        }
      }
      impdecls
    } finally {
      trace_return("import_stmts")
    }

  }

  def compilation_unit(): AST = {
    trace_call("compilation_unit")

    val t: Token = getToken(1)
    val impdecls: Declarations = import_stmts()
    val decls: Declarations = top_defs()
    decls
  }


  private def trace_call(str: String): Unit = {
    if (trace_enabled) {
      for (_ <- 0 until trace_indent) {
        print(" ")
      }
      println(s"Call:  $str")
    }
    trace_indent += 2
  }

  private def trace_return(str: String) = {
    trace_indent -= 2
    if (trace_enabled) {
      for (_ <- 0 until trace_indent)
        print(" ")
      println(s"Return: $str")
    }
  }

  private def trace_token(t: Token, where: String): Unit = {
    if (trace_enabled) {
      for (_ <- 0 until trace_indent)
        print(" ")
      println(s"Consumed token: < ${tokenImage(t.kind)}")
      if (t.kind != 0 && !tokenImage(t.kind) == s"\"${t.image}\"") {
        print(s": \"${t.image}\"")
      }
      println(s" at line ${t.beginLine} columa ${t.beginColumn}> where")
    }
  }

  var token_source: ParserTokenManager = _
  var input_stream: SimpleCharStream = _


  private var lookingAhead = false
  private var semLA = false
  private var gen = 0
  private val la1: Array[Int] = Array.ofDim(51)
  private var gc = 0
  private var rescan = false
  private val rtns: Array[JJCalls] = Array.ofDim(22)

  private var scanpos: Token = _
  private var lastpos: Token = _
  var token: Token = _
  var nt: Token = _
  private var ntk = 0
  private var la = 0

  private var kind = -1
  private val lasttokens = Array.ofDim[Int](100)
  private var endpos = 0
  private var expentry: ArrayBuffer[Int] = ArrayBuffer()

  final def getToken(i: Int) = {
    var t = if (lookingAhead) scanpos else token
    for (_ <- 0 until i) {
      if (t.next != null) t = t.next
      else {
        t.next = token_source.getNextToken()
        t = t.next
      }
    }
    t
  }


  private def p_2_19(xla:Int): Boolean ={
    la = xla
    scanpos = token
    lastpos = scanpos
    try {
      !p_3_19()
    } catch {
      case e:LookaheadSuccess =>
        true
    } finally {
      save(18,xla)
    }
  }

  private def p_3_19(xla:Int):Boolean = {
    if(p_3R_39()) return true
    if(scan_token(49)) return true
    false
  }


}

object Parser {

  def parseFile(file: File, loader: LibraryLoader, errorHandler: ErrorHandler) = {
    parseFile(file, loader, errorHandler, false).parse()
  }

  val SOURCE_ENCODING = "UTF-8"

  def newFileParser(file: File, loader: Any, errorHandler: ErrorHandler, debug: Boolean) = {
    try {
      val r = new BufferedReader(
        new InputStreamReader(
          new FileInputStream(file), SOURCE_ENCODING))
      new Parser(r, file.getPath, loader, errorHandler, debug)
    }
  }


  def parseFile(file: File, loader: LibraryLoader, errorHandler: ErrorHandler, debug: Boolean) = {
    newFileParser(file, loader, errorHandler, debug).parse()
  }


  final class JJCalls {
    var cgen = 0
    var first: Token = _
    var arg = 0
    var next: JJCalls = _
  }


}
