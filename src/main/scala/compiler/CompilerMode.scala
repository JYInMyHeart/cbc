package compiler

object CompilerMode extends Enumeration {
  type CompilerMode = Value
  val CheckSyntax = Value("--check-syntax")
  val DumpTokens = Value("--dump-tokens")
  val DumpAST = Value("--dump-ast")
  val DumpStmt = Value("--dump-stmt")
  val DumpExpr = Value("--dump-expr")
  val DumpSemantic = Value("--dump-semantic")
  val DumpReference = Value("--dump-reference")
  val DumpIR = Value("--dump-ir")
  val DumpAsm = Value("--dump-asm")
  val PrintAsm = Value("--print-asm")
  val Compile = Value("-S")
  val Assemble = Value("-c")
  val Link = Value("--link")

  private val modes = Map[String, CompilerMode](
    "--check-syntax" -> CheckSyntax,
    "  --dump-tokens" -> DumpTokens,
    "  --dump-ast" -> DumpAST,
    "  --dump-stmt" -> DumpStmt,
    "  --dump-expr" -> DumpExpr,
    "  --dump-semantic" -> DumpSemantic,
    "  --dump-reference" -> DumpReference,
    "  --dump-ir" -> DumpIR,
    "  --dump-asm" -> DumpAsm,
    "  --print-asm" -> PrintAsm,
    "  -S" -> Compile,
    "  -c" -> Assemble
  )





  def isModeOption(opt:String) = modes.contains(opt)

  def fromOption(opt:String) = modes(opt) match {
    case x => Some(x)
    case _ => None
  }
}

class CompilerMode(private val option:String){
  def toOption = option
}
