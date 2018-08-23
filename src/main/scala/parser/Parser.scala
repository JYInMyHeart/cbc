package parser

import java.io._

import ast._
import compiler.ErrorHandler
import entity.Declarations
import exception.{CompileException, SyntaxException}
import parser.Parser.JJCalls

import scala.collection.mutable.ArrayBuffer


class Parser extends ParserConstants {
  private var sourceName: String = _
  private var loader: LibraryLoader = _
  private var errorHandler: ErrorHandler = _
  private var knownTypedefs: Set[String] = _


  private var trace_enabled = true
  private var trace_indent = 0

  def this(r: Reader, name: String, loader: LibraryLoader, errorHandler: ErrorHandler, debug: Boolean) {
    this
    this.sourceName = name
    this.errorHandler = errorHandler
    this.loader = loader
    this.knownTypedefs = Set[String]()
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
    p_nt = token.next
    p_nt match {
      case null =>
        token.next = token_source.getNextToken()
        p_ntk = token.next.kind
      case _ =>
        p_ntk = p_nt.kind
    }
    p_ntk
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
    p_ntk = -1
    if (token.kind == kind) {
      p_gen += 1
      p_gc += 1
      if (p_gc > 100) {
        p_gc = 0
        var index = 0
        for (i <- p_rtns) {
          var c = i
          while (c != null) {
            if (c.cgen < p_gen) c.first = null
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

  def term(): ExprNode = {
    trace_call("term")
    try {
      var t: TypeNode = null
      var n: ExprNode = null
      if (p_2_21(Int.MinValue)) {
        consume_token(46)
        t = ctype()
        consume_token(51)
        n = term()
        return new CastNode(t, n)
      } else {
        (if (p_ntk == -1) getNtk() else p_ntk) match {
          case x if x == SIZEOF
            | x == IDENTIFIER
            | x == INTEGER
            | x == CHARACTER
            | x == STRING
            | x == 46
            | x == 57
            | x == 80
            | x == 83
            | x == 84
            | x == 87
            | x == 88
            | x == 89
            | x == 90 =>
            n = unary()
            return n
          case _ =>
            p_la1(43) = p_gen
            consume_token(-1)
            throw new ParseException("")
        }
      }
      throw new Error("Missing return statement in function")
    } finally trace_return("term")
  }

  def expr(): ExprNode = {
    trace_call("expr")
    try {
      var lhs: ExprNode = null
      var rhs: ExprNode = null
      var expr: ExprNode = null
      var op: String = ""
      if (p_2_19(Int.MinValue)) {
        lhs = term()
        consume_token(49)
        rhs = expr()
        return new AssignNode(lhs, rhs)
      } else if (p_2_20(Int.MinValue)) {
        lhs = term()
        op = opassign_op()
        rhs = expr()
        return new OpAssignNode(lhs, op, rhs)
      } else (if (p_ntk == -1) getNtk() else p_ntk) match {
        case x if x == SIZEOF
          | x == IDENTIFIER
          | x == INTEGER
          | x == CHARACTER
          | x == STRING
          | x == 46
          | x == 57
          | x == 80
          | x == 83
          | x == 84
          | x == 87
          | x == 88
          | x == 89
          | x == 90 =>
          expr = expr10()
          return expr
        case _ =>
          p_la1(27) = p_gen
          consume_token(-1)
          throw new ParseException("")
      }
      throw new Error("Missing return statement in function")
    } finally trace_return("expr")

  }

  def args() = {
    trace_call("args")
    try {
      var argss = ArrayBuffer[ExprNode]()
      var arg: ExprNode = null
      (if (p_ntk == -1) getNtk() else p_ntk) match {
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
            (if (p_ntk == -1) getNtk() else p_ntk) match {
              case 50 =>
              case _ =>
                p_la1(48) = p_gen
                failed = false
            }
            consume_token(50)
            arg = expr()
            argss.+=(arg)
          }
        case _ =>
          p_la1(50) = p_gen
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
        (if (p_ntk == -1) getNtk() else p_ntk) match {
          case 47 =>
          case _ =>
            p_la1(3) = p_gen
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

  private def addKnownTypedefs(typedefs: List[TypedefNode]) = {
    for (n <- typedefs)
      addType(n.name)
  }

  private def addType(name: String): Unit = {
    knownTypedefs += name
  }

  private def isType(name: String) = {
    knownTypedefs.contains(name)
  }

  def import_stmts(): Declarations = {
    trace_call("import_stmts")
    try {
      var libid: String = ""
      var impdecls: Declarations = new Declarations()
      var failed = true
      while (failed) {
        (if (p_ntk == -1) getNtk() else p_ntk) match {
          case IMPORT =>
          case _ =>
            p_la1(2) = p_gen
            failed = false
        }
        libid = import_stmt()
        try {
          val decls = loader.loadLibrary(libid, errorHandler)
          if (decls != null) {
            impdecls.add(decls)
            addKnownTypedefs(decls.typedefs.toList)
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


  private var p_lookingAhead = false
  private var p_semLA = false
  private var p_gen = 0
  private val p_la1: Array[Int] = Array.ofDim(51)
  private val p_la1_0 = Array[Int](0x80019800, 0x80019800, 0x0, 0x0, 0x80011800, 0x0, 0x0, 0x0, 0x4000, 0x0, 0x0, 0x0, 0x0, 0x7c0, 0x41800, 0x7f280000, 0x0, 0x7f280000, 0x100000, 0x0, 0x0, 0x0, 0x400000, 0x800000, 0x400000, 0x7f280000, 0x8000000, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  private val p_la1_1 = Array[Int](0x0, 0x0, 0x1, 0x8000, 0x0, 0x20000, 0x40000, 0x20000, 0x0, 0x40000, 0x2804000, 0x2804000, 0x40000, 0x0, 0x0, 0x221610e, 0x10000, 0x220610e, 0x0, 0x200610e, 0x200610e, 0x200610e, 0x0, 0x0, 0x0, 0x221610e, 0x0, 0x200610e, 0xf8000000, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x2000000, 0x2000000, 0x200610e, 0x2000000, 0x610e, 0x80c000, 0x80c000, 0x40000, 0x200610e, 0x610c)
  private val p_la1_2 = Array[Int](0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x7990000, 0x0, 0x7990000, 0x0, 0x7990000, 0x7990000, 0x7990000, 0x0, 0x0, 0x0, 0x7990000, 0x0, 0x7990000, 0x1f, 0x20, 0x40, 0x80, 0x3f00, 0x3f00, 0x4000, 0x8000, 0x10000, 0x60000, 0x60000, 0x180000, 0x180000, 0x600000, 0x600000, 0x7990000, 0x7990000, 0x0, 0x9800000, 0x9800000, 0x0, 0x7990000, 0x0)


  private var p_gc = 0
  private var p_rescan = false
  private val p_rtns: Array[JJCalls] = Array.ofDim(22)

  private var p_scanpos: Token = _
  private var p_lastpos: Token = _
  var token: Token = _
  var p_nt: Token = _
  private var p_ntk = 0
  private var p_la = 0

  private var kind = -1
  private val p_lasttokens = Array.ofDim[Int](100)
  private var p_endpos = 0
  private var p_expentry: Array[Int] = _

  final def getToken(i: Int) = {
    var t = if (p_lookingAhead) p_scanpos else token
    for (_ <- 0 until i) {
      if (t.next != null) t = t.next
      else {
        t.next = token_source.getNextToken()
        t = t.next
      }
    }
    t
  }

  private val p_ls = new LookaheadSuccess()

  private def p_scan_token(kind: Int): Boolean = {
    if (p_scanpos == p_lastpos) {
      p_la -= 1
      if (p_scanpos.next == null) {
        p_scanpos.next = token_source.getNextToken()
        p_scanpos = p_scanpos.next
        p_lastpos == p_scanpos
      } else {
        p_scanpos = p_scanpos.next
        p_lastpos == p_scanpos
      }
    } else {
      p_scanpos = p_scanpos.next
    }
    if (p_rescan) {
      var i = 0
      var tok = token
      while (tok != null && tok != p_scanpos) {
        i += 1
        tok = tok.next
      }
      if (tok != null) add_error_token(kind, i)
    }
    if (p_scanpos.kind != kind) return true
    if (p_la == 0 && p_scanpos == p_lastpos) throw p_ls
    false
  }

  private def add_error_token(kind: Int, pos: Int) = {
    pos match {
      case x if x >= 100 =>
      case x if x == p_endpos + 1 =>
        p_lasttokens(p_endpos) = kind
        p_endpos += 1
      case _ if p_endpos != 0 =>
        p_expentry = Array.ofDim[Int](p_endpos)
        for (i <- 0 until p_endpos) {
          p_expentry(i) = p_lasttokens(i)
        }

    }

  }

  private def p_save(index:Int,xla:Int) = {
    var failed = true
    var p = p_rtns(index)
    while(failed && p.cgen > p_gen){
      if(p.next == null){
        p.next = new JJCalls
        p = p.next
        failed = false
      }
      p = p.next
    }
    p.cgen = p_gen + xla - p_la
    p.first = token
    p.arg = xla
  }

  private def p_2_1(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_1
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(0, xla)
  }

  private def p_2_2(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_2
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(1, xla)
  }

  private def p_2_3(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_3
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(2, xla)
  }

  private def p_2_4(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_4
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(3, xla)
  }

  private def p_2_5(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_5
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(4, xla)
  }

  private def p_2_6(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_6
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(5, xla)
  }

  private def p_2_7(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_7
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(6, xla)
  }

  private def p_2_8(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_8
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(7, xla)
  }

  private def p_2_9(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_9
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(8, xla)
  }

  private def p_2_10(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_10
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(9, xla)
  }

  private def p_2_11(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_11
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(10, xla)
  }

  private def p_2_12(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_12
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(11, xla)
  }

  private def p_2_13(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_13
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(12, xla)
  }

  private def p_2_14(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_14
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(13, xla)
  }

  private def p_2_15(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_15
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(14, xla)
  }

  private def p_2_16(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_16
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(15, xla)
  }

  private def p_2_17(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_17
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(16, xla)
  }

  private def p_2_18(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_18
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(17, xla)
  }

  private def p_2_19(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_19
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(18, xla)
  }

  private def p_2_20(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_20
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(19, xla)
  }

  private def p_2_21(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_21
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(20, xla)
  }

  private def p_2_22(xla: Int) = {
    p_la = xla
    p_scanpos = token
    p_lastpos = p_scanpos
    try
      !p_3_22
    catch {
      case ls: LookaheadSuccess =>
        true
    } finally p_save(21, xla)
  }

  private def p_3R_96: Boolean = {
    if (p_scan_token(IDENTIFIER)) return true
    false
  }

  private def p_3R_117: Boolean = {
    if (p_scan_token(71)) return true
    if (p_3R_116) return true
    false
  }

  private def p_3R_116: Boolean = {
    var failed = true
    if (p_3R_118) return true
    while (failed) {
      var xsp = p_scanpos
      if (p_3R_119) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3R_95: Boolean = {
    if (p_scan_token(STRING)) return true
    false
  }

  private def p_3R_75: Boolean = {
    if (p_scan_token(46)) return true
    if (p_3R_77) return true
    if (p_scan_token(51)) return true
    false
  }

  private def p_3R_32 = {
    var xsp = p_scanpos
    if (p_scan_token(14)) p_scanpos = xsp
    false
  }

  private def p_3R_94: Boolean = {
    if (p_scan_token(CHARACTER)) return true
    false
  }

  private def p_3R_74: Boolean = {
    if (p_scan_token(57)) return true
    false
  }

  private def p_3R_115: Boolean = {
    if (p_scan_token(70)) return true
    if (p_3R_114) return true
    false
  }

  private def p_3R_114: Boolean = {
    var failed = true
    if (p_3R_116) return true
    while ( {
      failed
    }) {
      var xsp = p_scanpos
      if (p_3R_117) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3R_93: Boolean = {
    if (p_scan_token(INTEGER)) return true
    false
  }

  private def p_3R_91: Boolean = {
    var xsp = p_scanpos
    if (p_3R_93) {
      p_scanpos = xsp
      if (p_3R_94) {
        p_scanpos = xsp
        if (p_3R_95) {
          p_scanpos = xsp
          if (p_3R_96) {
            p_scanpos = xsp
            if (p_3R_97) return true
          }
        }
      }
    }
    false
  }

  private def p_3R_73: Boolean = {
    if (p_scan_token(55)) return true
    if (p_scan_token(INTEGER)) return true
    if (p_scan_token(56)) return true
    false
  }

  private def p_3_10: Boolean = {
    if (p_scan_token(55)) return true
    if (p_scan_token(56)) return true
    false
  }

  private def p_3R_43: Boolean = {
    val xsp = p_scanpos
    if (p_3_10) {
      p_scanpos = xsp
      if (p_3R_73) {
        p_scanpos = xsp
        if (p_3R_74) {
          p_scanpos = xsp
          if (p_3R_75) return true
        }
      }
    }
    false
  }

  private def p_3R_44: Boolean = {
    if (p_3R_32) return true
    if (p_3R_26) return true
    false
  }

  private def p_3R_50: Boolean = {
    if (p_scan_token(49)) return true
    false
  }

  private def p_3R_26: Boolean = {
    var failed = true
    if (p_3R_42) return true
    while (failed) {
     val xsp = p_scanpos
      if (p_3R_43) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3R_112: Boolean = {
    var failed = true
    if (p_3R_114) return true
    while (failed) {
     val xsp = p_scanpos
      if (p_3R_115) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3R_113: Boolean = {
    if (p_scan_token(69)) return true
    if (p_3R_104) return true
    if (p_scan_token(58)) return true
    if (p_3R_110) return true
    false
  }

  private def p_3R_111: Boolean = {
    if (p_scan_token(50)) return true
    if (p_3R_104) return true
    false
  }

  private def p_3R_109: Boolean = {
    var failed = true
    if (p_3R_104) return true

    while (failed) {
      val xsp = p_scanpos
      if (p_3R_111) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3R_105 = {
    val xsp = p_scanpos
    if (p_3R_109) p_scanpos = xsp
    false
  }

  private def p_3R_41: Boolean = {
    if (p_3R_26) return true
    false
  }

  private def p_3R_110: Boolean = {
    if (p_3R_112) return true
    val xsp = p_scanpos
    if (p_3R_113) p_scanpos = xsp
    false
  }

  private def p_3R_45: Boolean = {
    if (p_scan_token(CONST)) return true
    false
  }

  private def p_3R_49: Boolean = {
    if (p_scan_token(IDENTIFIER)) return true
    false
  }

  private def p_3R_63: Boolean = {
    if (p_scan_token(68)) return true
    false
  }

  private def p_3R_62: Boolean = {
    if (p_scan_token(67)) return true
    false
  }

  private def p_3R_61: Boolean = {
    if (p_scan_token(66)) return true
    false
  }

  private def p_3R_60: Boolean = {
    if (p_scan_token(65)) return true
    false
  }

  private def p_3R_59: Boolean = {
    if (p_scan_token(64)) return true
    false
  }

  private def p_3R_58: Boolean = {
    if (p_scan_token(63)) return true
    false
  }

  private def p_3R_57: Boolean = {
    if (p_scan_token(62)) return true
    false
  }

  private def p_3R_56: Boolean = {
    if (p_scan_token(61)) return true
    false
  }

  private def p_3R_55: Boolean = {
    if (p_scan_token(60)) return true
    false
  }

  private def p_3R_103: Boolean = {
    if (p_scan_token(46)) return true
    if (p_3R_105) return true
    if (p_scan_token(51)) return true
    false
  }

  private def p_3R_54: Boolean = {
    if (p_scan_token(59)) return true
    false
  }

  private def p_3R_102: Boolean = {
    if (p_scan_token(91)) return true
    if (p_3R_49) return true
    false
  }

  private def p_3R_101: Boolean = {
    if (p_scan_token(47)) return true
    if (p_3R_49) return true
    false
  }

  private def p_3R_40: Boolean = {
    val xsp = p_scanpos
    if (p_3R_54) {
      p_scanpos = xsp
      if (p_3R_55) {
        p_scanpos = xsp
        if (p_3R_56) {
          p_scanpos = xsp
          if (p_3R_57) {
            p_scanpos = xsp
            if (p_3R_58) {
              p_scanpos = xsp
              if (p_3R_59) {
                p_scanpos = xsp
                if (p_3R_60) {
                  p_scanpos = xsp
                  if (p_3R_61) {
                    p_scanpos = xsp
                    if (p_3R_62) {
                      p_scanpos = xsp
                      if (p_3R_63) return true
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    false
  }

  private def p_3R_100: Boolean = {
    if (p_scan_token(55)) return true
    if (p_3R_104) return true
    if (p_scan_token(56)) return true
    false
  }

  private def p_3R_99: Boolean = {
    if (p_scan_token(88)) return true
    false
  }

  private def p_3R_51: Boolean = {
    if (p_scan_token(50)) return true
    false
  }

  private def p_3R_98: Boolean = {
    if (p_scan_token(87)) return true
    false
  }

  private def p_3R_92: Boolean = {
    val xsp = p_scanpos
    if (p_3R_98) {
      p_scanpos = xsp
      if (p_3R_99) {
        p_scanpos = xsp
        if (p_3R_100) {
          p_scanpos = xsp
          if (p_3R_101) {
            p_scanpos = xsp
            if (p_3R_102) {
              p_scanpos = xsp
              if (p_3R_103) return true
            }
          }
        }
      }
    }
    false
  }

  private def p_3_20: Boolean = {
    if (p_3R_39) return true
    if (p_3R_40) return true
    false
  }

  private def p_3R_89: Boolean = {
    var failed = true
    if (p_3R_91) return true
    while (failed) {
      val xsp = p_scanpos
      if (p_3R_92) {
        p_scanpos = xsp
        failed = false //todo: break is not supported
      }
    }
    false
  }

  private def p_3R_108: Boolean = {
    if (p_3R_110) return true
    false
  }

  private def p_3_19: Boolean = {
    if (p_3R_39) return true
    if (p_scan_token(49)) return true
    false
  }

  private def p_3R_33: Boolean = {
    var failed = true
    if (p_3R_32) return true
    if (p_3R_41) return true
    if (p_3R_49) return true
    var xsp = p_scanpos
    if (p_3R_50) p_scanpos = xsp
    while (failed) {
      xsp = p_scanpos
      if (p_3R_51) {
        p_scanpos = xsp
        failed = false //todo: break is not supported
      }
    }
    if (p_scan_token(48)) return true
    false
  }

  private def p_3R_38: Boolean = {
    if (p_scan_token(IDENTIFIER)) return true
    if (p_scan_token(58)) return true
    false
  }

  private def p_3R_107: Boolean = {
    if (p_3R_39) return true
    if (p_3R_40) return true
    if (p_3R_104) return true
    false
  }

  private def p_3R_87: Boolean = {
    if (p_3R_89) return true
    false
  }

  private def p_3R_36: Boolean = {
    if (p_3R_41) return true
    false
  }

  private def p_3R_106: Boolean = {
    if (p_3R_39) return true
    if (p_scan_token(49)) return true
    if (p_3R_104) return true
    false
  }

  private def p_3R_104: Boolean = {
    val xsp = p_scanpos
    if (p_3R_106) {
      p_scanpos = xsp
      if (p_3R_107) {
        p_scanpos = xsp
        if (p_3R_108) return true
      }
    }
    false
  }

  private def p_3R_86: Boolean = {
    if (p_scan_token(SIZEOF)) return true
    if (p_3R_76) return true
    false
  }

  private def p_3_3: Boolean = {
    if (p_3R_32) return true
    if (p_3R_26) return true
    if (p_scan_token(IDENTIFIER)) return true
    if (p_scan_token(46)) return true
    false
  }

  private def p_3_22: Boolean = {
    if (p_scan_token(SIZEOF)) return true
    if (p_scan_token(46)) return true
    if (p_3R_41) return true
    if (p_scan_token(51)) return true
    false
  }

  private def p_3R_85: Boolean = {
    if (p_scan_token(80)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3R_31: Boolean = {
    if (p_3R_48) return true
    false
  }

  private def p_3R_84: Boolean = {
    if (p_scan_token(57)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3R_30: Boolean = {
    if (p_3R_47) return true
    false
  }

  private def p_3_9: Boolean = {
    if (p_3R_36) return true
    false
  }

  private def p_3R_83: Boolean = {
    if (p_scan_token(90)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3R_29: Boolean = {
    if (p_3R_46) return true
    false
  }

  private def p_3R_82: Boolean = {
    if (p_scan_token(89)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3R_28: Boolean = {
    if (p_3R_45) return true
    false
  }

  private def p_3R_81: Boolean = {
    if (p_scan_token(84)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3_18: Boolean = {
    if (p_scan_token(RETURN)) return true
    if (p_scan_token(48)) return true
    false
  }

  private def p_3R_80: Boolean = {
    if (p_scan_token(83)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3_4: Boolean = {
    if (p_3R_33) return true
    false
  }

  private def p_3R_79: Boolean = {
    if (p_scan_token(88)) return true
    if (p_3R_76) return true
    false
  }

  private def p_3R_78: Boolean = {
    if (p_scan_token(87)) return true
    if (p_3R_76) return true
    false
  }

  private def p_3R_76: Boolean = {
    val xsp = p_scanpos
    if (p_3R_78) {
      p_scanpos = xsp
      if (p_3R_79) {
        p_scanpos = xsp
        if (p_3R_80) {
          p_scanpos = xsp
          if (p_3R_81) {
            p_scanpos = xsp
            if (p_3R_82) {
              p_scanpos = xsp
              if (p_3R_83) {
                p_scanpos = xsp
                if (p_3R_84) {
                  p_scanpos = xsp
                  if (p_3R_85) {
                    p_scanpos = xsp
                    if (p_3_22) {
                      p_scanpos = xsp
                      if (p_3R_86) {
                        p_scanpos = xsp
                        if (p_3R_87) return true
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    false
  }

  private def p_3R_27: Boolean = {
    if (p_3R_44) return true
    false
  }

  private def p_3_2: Boolean = {
    val xsp = p_scanpos
    if (p_3R_27) {
      p_scanpos = xsp
      if (p_3_4) {
        p_scanpos = xsp
        if (p_3R_28) {
          p_scanpos = xsp
          if (p_3R_29) {
            p_scanpos = xsp
            if (p_3R_30) {
              p_scanpos = xsp
              if (p_3R_31) return true
            }
          }
        }
      }
    }
    false
  }

  private def p_3_17: Boolean = {
    if (p_3R_38) return true
    false
  }

  private def p_3_21: Boolean = {
    if (p_scan_token(46)) return true
    if (p_3R_41) return true
    false
  }

  private def p_3R_53: Boolean = {
    if (p_3R_76) return true
    false
  }

  private def p_3R_47: Boolean = {
    if (p_scan_token(UNION)) return true
    false
  }

  private def p_3R_39: Boolean = {
    val xsp = p_scanpos
    if (p_3R_52) {
      p_scanpos = xsp
      if (p_3R_53) return true
    }
    false
  }

  private def p_3R_52: Boolean = {
    if (p_scan_token(46)) return true
    if (p_3R_41) return true
    if (p_scan_token(51)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3R_143: Boolean = {
    if (p_scan_token(86)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3R_142: Boolean = {
    if (p_scan_token(85)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3R_141: Boolean = {
    if (p_scan_token(57)) return true
    if (p_3R_39) return true
    false
  }

  private def p_3R_138: Boolean = {
    val xsp = p_scanpos
    if (p_3R_141) {
      p_scanpos = xsp
      if (p_3R_142) {
        p_scanpos = xsp
        if (p_3R_143) return true
      }
    }
    false
  }

  private def p_3R_46: Boolean = {
    if (p_scan_token(STRUCT)) return true
    false
  }

  private def p_3R_140: Boolean = {
    if (p_scan_token(84)) return true
    if (p_3R_134) return true
    false
  }

  private def p_3R_134: Boolean = {
    var failed = true
    if (p_3R_39) return true
    while (failed) {
      val xsp = p_scanpos
      if (p_3R_138) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3R_139: Boolean = {
    if (p_scan_token(83)) return true
    if (p_3R_134) return true
    false
  }

  private def p_3R_135: Boolean = {
    val xsp = p_scanpos
    if (p_3R_139) {
      p_scanpos = xsp
      if (p_3R_140) return true
    }
    false
  }

  private def p_3R_48: Boolean = {
    if (p_scan_token(TYPEDEF)) return true
    false
  }

  private def p_3_8: Boolean = {
    if (p_3R_33) return true
    false
  }

  private def p_3R_137: Boolean = {
    if (p_scan_token(82)) return true
    if (p_3R_132) return true
    false
  }

  private def p_3R_132: Boolean = {
    var failed = true
    if (p_3R_134) return true
    while (failed) {
      val xsp = p_scanpos
      if (p_3R_135) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3R_136: Boolean = {
    if (p_scan_token(81)) return true
    if (p_3R_132) return true
    false
  }

  private def p_3R_133: Boolean = {
    val xsp = p_scanpos
    if (p_3R_136) {
      p_scanpos = xsp
      if (p_3R_137) return true
    }
    false
  }

  private def p_3R_72: Boolean = {
    if (p_scan_token(IDENTIFIER)) return true
    false
  }

  private def p_3R_71: Boolean = {
    if (p_scan_token(UNION)) return true
    if (p_scan_token(IDENTIFIER)) return true
    false
  }

  private def p_3R_70: Boolean = {
    if (p_scan_token(STRUCT)) return true
    if (p_scan_token(IDENTIFIER)) return true
    false
  }

  private def p_3R_69: Boolean = {
    if (p_scan_token(UNSIGNED)) return true
    if (p_scan_token(LONG)) return true
    false
  }

  private def p_3_16: Boolean = {
    if (p_scan_token(UNSIGNED)) return true
    if (p_scan_token(INT)) return true
    false
  }

  private def p_3_15: Boolean = {
    if (p_scan_token(UNSIGNED)) return true
    if (p_scan_token(SHORT)) return true
    false
  }

  private def p_3R_131: Boolean = {
    if (p_scan_token(80)) return true
    if (p_3R_130) return true
    false
  }

  private def p_3R_130: Boolean = {
    var failed = true
    if (p_3R_132) return true
    while (failed) {
      val xsp = p_scanpos
      if (p_3R_133) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3_14: Boolean = {
    if (p_scan_token(UNSIGNED)) return true
    if (p_scan_token(CHAR)) return true
    false
  }

  private def p_3R_68: Boolean = {
    if (p_scan_token(LONG)) return true
    false
  }

  private def p_3_1: Boolean = {
    if (p_scan_token(EXTERN)) return true
    if (p_3R_26) return true
    if (p_scan_token(IDENTIFIER)) return true
    if (p_scan_token(46)) return true
    false
  }

  private def p_3R_67: Boolean = {
    if (p_scan_token(INT)) return true
    false
  }

  private def p_3R_66: Boolean = {
    if (p_scan_token(SHORT)) return true
    false
  }

  private def p_3R_65: Boolean = {
    if (p_scan_token(CHAR)) return true
    false
  }

  private def p_3R_42: Boolean = {
    val xsp = p_scanpos
    if (p_3R_64) {
      p_scanpos = xsp
      if (p_3R_65) {
        p_scanpos = xsp
        if (p_3R_66) {
          p_scanpos = xsp
          if (p_3R_67) {
            p_scanpos = xsp
            if (p_3R_68) {
              p_scanpos = xsp
              if (p_3_14) {
                p_scanpos = xsp
                if (p_3_15) {
                  p_scanpos = xsp
                  if (p_3_16) {
                    p_scanpos = xsp
                    if (p_3R_69) {
                      p_scanpos = xsp
                      if (p_3R_70) {
                        p_scanpos = xsp
                        if (p_3R_71) {
                          p_scanpos = xsp
                          p_lookingAhead = true
                          p_semLA = isType(getToken(1).image)
                          p_lookingAhead = false
                          if (!p_semLA || p_3R_72) return true
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    false
  }

  private def p_3R_64: Boolean = {
    if (p_scan_token(VOID)) return true
    false
  }

  private def p_3R_35: Boolean = {
    if (p_3R_41) return true
    false
  }

  private def p_3R_129: Boolean = {
    if (p_scan_token(79)) return true
    if (p_3R_128) return true
    false
  }

  private def p_3R_128: Boolean = {
    var failed = true
    if (p_3R_130) return true
    while (failed) {
      val xsp = p_scanpos
      if (p_3R_131) {
        p_scanpos = xsp
        failed = false //todo: break is not supported
      }
    }
    false
  }

  private def p_3_13: Boolean = {
    if (p_scan_token(50)) return true
    if (p_3R_26) return true
    false
  }

  private def p_3R_37: Boolean = {
    var failed = true
    if (p_3R_26) return true
    while (failed) {
      val xsp = p_scanpos
      if (p_3_13) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3R_121: Boolean = {
    if (p_scan_token(78)) return true
    if (p_3R_120) return true
    false
  }

  private def p_3R_120: Boolean = {
    var failed = true
    if (p_3R_128) return true
    while (failed) {
      val xsp = p_scanpos
      if (p_3R_129) {
        p_scanpos = xsp
        failed = false //todo: break is not supported
      }
    }
    false
  }

  private def p_3_7: Boolean = {
    if (p_scan_token(50)) return true
    if (p_3R_35) return true
    false
  }

  private def p_3R_34: Boolean = {
    if (p_3R_35) return true
    false
  }

  private def p_3R_90: Boolean = {
    if (p_scan_token(50)) return true
    if (p_scan_token(52)) return true
    false
  }

  private def p_3_11: Boolean = {
    if (p_scan_token(VOID)) return true
    if (p_scan_token(51)) return true
    false
  }

  private def p_3R_127: Boolean = {
    if (p_scan_token(77)) return true
    if (p_3R_118) return true
    false
  }

  private def p_3R_126: Boolean = {
    if (p_scan_token(76)) return true
    if (p_3R_118) return true
    false
  }

  private def p_3R_118: Boolean = {
    var failed = true
    if (p_3R_120) return true
    while (failed) {
      val xsp = p_scanpos
      if (p_3R_121) {
        p_scanpos = xsp
        failed = false
      }
    }
    false
  }

  private def p_3_5: Boolean = {
    if (p_scan_token(VOID)) return true
    if (p_scan_token(51)) return true
    false
  }

  private def p_3R_125: Boolean = {
    if (p_scan_token(75)) return true
    if (p_3R_118) return true
    false
  }

  private def p_3_12: Boolean = {
    if (p_3R_37) return true
    val xsp = p_scanpos
    if (p_3R_90) p_scanpos = xsp
    false
  }

  private def p_3R_124: Boolean = {
    if (p_scan_token(74)) return true
    if (p_3R_118) return true
    false
  }

  private def p_3R_123: Boolean = {
    if (p_scan_token(73)) return true
    if (p_3R_118) return true
    false
  }

  private def p_3R_122: Boolean = {
    if (p_scan_token(72)) return true
    if (p_3R_118) return true
    false
  }

  private def p_3R_119: Boolean = {
    val xsp = p_scanpos
    if (p_3R_122) {
      p_scanpos = xsp
      if (p_3R_123) {
        p_scanpos = xsp
        if (p_3R_124) {
          p_scanpos = xsp
          if (p_3R_125) {
            p_scanpos = xsp
            if (p_3R_126) {
              p_scanpos = xsp
              if (p_3R_127) return true
            }
          }
        }
      }
    }
    false
  }

  private def p_3_6: Boolean = {
    if (p_3R_34) return true
    false
  }

  private def p_3R_88: Boolean = {
    if (p_scan_token(VOID)) return true
    false
  }

  private def p_3R_77: Boolean = {
    val xsp = p_scanpos
    if (p_3R_88) {
      p_scanpos = xsp
      if (p_3_12) return true
    }
    false
  }

  private def p_3R_97: Boolean = {
    if (p_scan_token(46)) return true
    if (p_3R_104) return true
    if (p_scan_token(51)) return true
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
