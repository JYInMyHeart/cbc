package ast

import ctype.TypeRef

abstract class CompositeTypeDefinition extends TypeDefinition {
  var members:List[Slot] = _

  def this(loc: Location, ref: TypeRef, name: String, membs: List[Slot]) = {
    this
    this.members = membs
    this.typeNode = new TypeNode(ref)
    this.name = name
    this.location = loc
  }

  def isCompositeType = true

  abstract def kind():String
}
