package ast

import ctype.TypeRef

abstract class TypeDefinition extends Node{
  var name:String = _
  var location:Location = _
  var typeNode:TypeNode = _

  def this(location: Location,ref:TypeRef,name:String) = {
    this
    this.name = name
    this.location = location
    this.typeNode = new TypeNode(ref)
  }

}
