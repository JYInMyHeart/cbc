package entity
import ast.{BlockNode, Dumper}
import ir.Stmt

class DefinedFunction extends Function {
//  var params:Params = _
  var body:BlockNode = _
  var scope:LocalScope = _
  var ir:List[Stmt] = _
  override def dump(d: Dumper): Unit = ???
}
