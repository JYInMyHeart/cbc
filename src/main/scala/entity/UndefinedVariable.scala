package entity
import ast.{Dumper, TypeNode}

class UndefinedVariable extends Variable {
  def this(t:TypeNode,name:String) = {
    this
    this.name = name
    this.isPrivate = false
    this.typeNode = t
    this.nRefered = 0

  }
  override def dump(d: Dumper): Unit = ???
}
