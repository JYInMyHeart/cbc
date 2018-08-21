package entity
import ast.{Dumper, ExprNode, TypeNode}
import ctype.Ctype
import ir.Expr

class DefinedVariable extends Variable {
  var initializer:ExprNode = _
  var ir:Expr = _
  var sequence:Long = _
//  var symbol:Symbol = _

  def this(priv: Boolean, t: TypeNode,
           name: String, init: ExprNode) = {
    this
    this.isPrivate = priv
    this.typeNode = t
    this.name = name
    this.nRefered = 0
    this.initializer = init
    this.sequence = -1
  }
  override def dump(d: Dumper): Unit = ???


}

object DefinedVariable{
  private var tmqSeq:Long = 0

  def tmp(t:Ctype) = {
    new DefinedVariable(false,new TypeNode(t),s"@tmp${tmqSeq += 1}",null)
  }
}
