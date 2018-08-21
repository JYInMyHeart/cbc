package ast

import ctype.{Ctype, TypeRef}

class TypeNode extends Node{
  var typeRef:TypeRef = _
  var ctype:Ctype = _

  def this(t:Ctype) = {
    this
    this.ctype = t
  }

  def this(ref:TypeRef) = {
    this
    this.typeRef = ref
  }

  override def location(): Location = ???

  override def dump(d: Dumper): Unit = ???
}
