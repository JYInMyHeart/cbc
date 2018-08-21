package ast

import entity.{DefinedVariable, LocalScope}

class BlockNode extends StmtNode {
  var variables:List[DefinedVariable] = _
  var stmts:List[StmtNode] = _
  var scope:LocalScope = _

  def this(loc:Location,vars:List[DefinedVariable],stmts:List[StmtNode]) = {
    this
    this.location = loc
    this.variables = vars
    this.stmts = stmts
  }

  override var location: Location = _

  override def dump(d: Dumper): Unit = ???
}
