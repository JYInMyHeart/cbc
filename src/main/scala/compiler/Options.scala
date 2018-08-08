package compiler

import compiler.CompilerMode.{fromOption, isModeOption}

class Options {
  var mode:CompilerMode.CompilerMode = _
  var outputFileName:String = _
  var verbose:Boolean = false
  var debugParser = false
  var sourceFiles:List[SourceFile] = _


  def parseArgs(origArgs:Array[String]) = {
    var sourceFiles = List[SourceFile]()
    var ldArgs = List[LdArg]()
    origArgs.foreach {
      case "--" =>
      case x@z if z startsWith "-" =>
        z match {
          case y if isModeOption(y) =>
            mode = fromOption(y).get
          case y if y == "--debug-parser" =>
            debugParser = true
          case y if y == "--version" =>
            println(s"${Compiler.programName} version ${Compiler.version}")
            System.exit(0)
          case _ =>
            parseError(s"unknown option $x")
        }
      case z =>
        ldArgs :+= new SourceFile(z)
      case _ =>
    }


  }

  def parseError(msg:String) = println(s"error:$msg")

  def apply: Options = new Options()
}
object Options{
  def parse(args:Array[String]) = {
    val opts = new Options()
    opts.parseArgs(args)
    opts
  }
}
