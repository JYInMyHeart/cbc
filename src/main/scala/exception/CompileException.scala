package exception

class CompileException(val msg:String) extends Exception(msg)

case class SyntaxException(override val msg:String) extends CompileException(msg)