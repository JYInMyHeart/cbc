package ast

import ctype.{TypeRef, UserTypeRef}

class TypedefNode extends TypeDefinition {
  var real:TypeNode = _

  def this(location: Location,real:TypeRef,name:String) = {
    this
    this.location = location
    this.real = new TypeNode(real)
    this.typeNode = new TypeNode(new UserTypeRef(name))
    this.name = name
  }
  override def dump(d: Dumper): Unit = ???
}
