package ast

class OpAssignNode extends AbstractAssignNode {
  var operator:String =  _
  def this(lhs:ExprNode,rhs:ExprNode,op:String) = {
    this
    this.lhs = lhs
    this.rhs = rhs
    this.operator = op
  }
}
