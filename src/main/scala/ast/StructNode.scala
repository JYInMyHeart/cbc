package ast

import ctype.TypeRef

class StructNode extends CompositeTypeDefinition {
  def this(loc: Location, ref: TypeRef, name: String, membs: List[Slot]) = {
    this
    this.members = membs
    this.location = loc
    this.typeNode = new TypeNode(ref)
    this.name = name
  }
  override def kind(): String = "struct"

  def isStruct = true

  override def dump(d: Dumper): Unit = ???
}
