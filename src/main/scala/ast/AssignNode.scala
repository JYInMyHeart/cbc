package ast

class AssignNode extends AbstractAssignNode {
  def this(lhs:ExprNode,rhs:ExprNode) = {
    this
    this.lhs = lhs
    this.rhs = rhs
  }

}
