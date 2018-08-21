package entity
import ast.{Dumper, ExprNode, TypeNode}

class Constant extends Entity {
  var value:ExprNode = _

  def this(t:TypeNode,name:String,value:ExprNode) = {
    this
    this.isPrivate = true
    this.typeNode = t
    this.name = name
    this.nRefered = 0
    this.value = value
  }
  override def dump(d: Dumper): Unit = ???
}
