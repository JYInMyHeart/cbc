package entity

import ast.TypeNode

abstract class Function extends Entity {
  //  var callingSymbol:Symbol = _
  //  var label:Label = _
  def this(priv: Boolean, t: TypeNode, name: String) = {
    this
    this.isPrivate = priv
    this.name = name
    this.typeNode = t
    this.nRefered = 0
  }

  def isInitialized = true
}
