package ast

abstract class AbstractAssignNode extends ExprNode {
  var lhs:ExprNode = _
  var rhs:ExprNode = _

  def this(lhs:ExprNode,rhs:ExprNode) = {
    this
    this.lhs = lhs
    this.rhs = rhs
  }

  override def location: Location = lhs.location

  override protected def dump(d: Dumper): Unit = {
//    d.printMember("lhs", lhs)
//    d.printMember("rhs", rhs)
  }
}
