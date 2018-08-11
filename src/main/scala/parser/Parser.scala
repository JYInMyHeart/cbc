package parser

import java.io._

import ast.AST
import compiler.{ErrorHandler, SyntaxException}
import entity.Declarations

class Parser{
  private var sourceName:String = _
  private var loader:LibraryLoader = _
  private var errorHandler:ErrorHandler = _
  private var knownTypeders:Set[String] = _



  private var trace_enabled = true
  private var trace_indent = 0

  def this(r:Reader,name:String,loader:LibraryLoader,errorHandler: ErrorHandler,debug:Boolean){
    this
    this.sourceName = name
    this.errorHandler = errorHandler
    this.loader = loader
    this.knownTypeders = Set[String]()
    if(debug)
      enable_tracing
    else
      disable_tracing
  }

  def enable_tracing = trace_enabled = true

  def disable_tracing = trace_enabled = false


  final private class LookaheadSuccess extends Error

  def parse():AST = {

      try {
        compilation_unit()
      } catch {
        case err:TokenMgrError =>
          throw  SyntaxException(err.getMessage)
        case ex:ParseException =>
          throw  SyntaxException(ex.getMessage)
        case err:LookaheadSuccess =>
          throw  SyntaxException("syntax error")
      }

  }

  def compilation_unit():AST = {
    trace_call("compilation_unit")

    val t:Token = getToken(1)
    val decls:Declarations = top_defs()
    val impdecls:Declarations = import_stmts()

    decls
  }


  private def trace_call(str: String): Unit ={
    if(trace_enabled){
      for (_ <- 0 until trace_indent){
        print(" ")
      }
      println(s"Call:  $str")
    }
    trace_indent += 2
  }

  var token_source:ParserTokenManager = _
  var input_stream:SimpleCharStream = _


  private var lookingAhead = false
  private var semLA = false
  private var gen = 0
  private val la1:Array[Int] = Array.ofDim(51)

  private var scanpos:Token = _
  private var lastpos:Token = _
  var token:Token = _
  private var ntk = 0
  private var la = 0

  final def getToken(i: Int) = {
    var t = if(lookingAhead) scanpos else token
    for(_ <- 0 until i){
      if(t.next != null) t = t.next
      else t = t.next = token_source.getNextToken()
    }
  }



}
object Parser {

  def parseFile(file:File,loader:LibraryLoader, errorHandler:ErrorHandler) = {
    parseFile(file,loader,errorHandler,false).parse()
  }

  val SOURCE_ENCODING = "UTF-8"

  def newFileParser(file: File, loader: Any, errorHandler: ErrorHandler, debug: Boolean) = {
    try {
      val r = new BufferedReader(
        new InputStreamReader(
          new FileInputStream(file), SOURCE_ENCODING))
      new Parser(r,file.getPath,loader,errorHandler,debug)
    }
  }



  def parseFile(file:File, loader:LibraryLoader, errorHandler:ErrorHandler, debug:Boolean) = {
    newFileParser(file,loader,errorHandler,debug).parse()
  }


}
