package compiler

class CompileException(val msg:String) extends Exception

case class SyntaxException(override val msg:String) extends CompileException(msg)