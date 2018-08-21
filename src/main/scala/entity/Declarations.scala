package entity

import ast.{StructNode, TypedefNode, UnionNode}

class Declarations {
  var defvars = Set[DefinedVariable]()
  var vardecls = Set[UndefinedVariable]()
  var defuns = Set[DefinedFunction]()
  var funcdecl = Set[UndefinedFunction]()
  var constant = Set[Constant]()
  var defstrucs = Set[StructNode]()
  var defunion = Set[UnionNode]()
  var typedefs = Set[TypedefNode]()

  def add(decls: Declarations) = {
    defvars ++= decls.defvars
    vardecls ++= decls.vardecls
    defuns ++= decls.defuns
    funcdecl ++= decls.funcdecl
    constant ++= decls.constant
    defstrucs ++= decls.defstrucs
    defunion ++= decls.defunion
    typedefs ++= decls.typedefs
  }

}
