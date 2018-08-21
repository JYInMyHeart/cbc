package exception

class CompileException(val msg:String) extends Exception(msg)

case class SyntaxException(override val msg:String) extends CompileException(msg)
case class FileException(override val msg:String) extends CompileException(msg)
case class SemanticException(override val msg:String) extends CompileException(msg)