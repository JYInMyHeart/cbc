package ast

import ctype.TypeRef

class UnionNode extends CompositeTypeDefinition {

  def this(loc: Location, ref: TypeRef, name: String, membs: List[Slot]) = {
    this
    this.members = membs
    this.location = loc
    this.typeNode = new TypeNode(ref)
    this.name = name
  }

  def isUnion = true
  override  def kind(): String = "union"

  override def dump(d: Dumper): Unit = ???
}
