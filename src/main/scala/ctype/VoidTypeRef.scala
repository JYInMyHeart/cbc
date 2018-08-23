package ctype

import ast.Location

class VoidTypeRef extends TypeRef {
  def this(loc:Location) = {
    this
    this.location = loc
  }
}
