package compiler

import java.io.{File, FileNotFoundException}

import ast.AST
import parser.Parser

class Compiler {
  private var errorHandler:ErrorHandler = _
  def this(program:String) = {
    this
    this.errorHandler = new ErrorHandler(program)
  }

  def commandMain(args:Array[String]):Unit = {
    val opts = parseOptions(args)
    if(opts.mode == CompilerMode.CheckSyntax)
      System.exit(if(checkSyntax(opts)) 0 else 1)
    build(opts.sourceFiles,opts)
    System.exit(0)
  }

  private def parseOptions(args:Array[String]) =
    Options.parse(args)

  private def checkSyntax(opts:Options):Boolean = {
    var failed = false
    opts.sourceFiles.foreach(x => failed &= isValidSyntax(x.path,opts))
    !failed
  }

  private def isValidSyntax(path:String,opts:Options) = {
    try {
      parseFile(path, opts)
      true
    } catch {
      case _:SyntaxException =>
        false
      case e:FileNotFoundException =>
        errorHandler.error(e.getMessage)
        false
    }
  }

  def parseFile(str: String, options: Options):AST =
    Parser.parseFile(new File(str),options.loader(),errorHandler,options.doseDebugParser())


}
object Compiler{
  val programName = "cbc"
  val version = "1.0"

}


