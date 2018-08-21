package entity

import ast.TypeNode

abstract class Variable extends Entity {
  def this(priv: Boolean, ctype: TypeNode, name: String) = {
    this
    this.isPrivate = priv
    this.typeNode = ctype
    this.name = name
    this.nRefered = 0
  }
}
