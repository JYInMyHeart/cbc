package ctype

import ast.Location

class UnionTypeRef extends TypeRef {
  var name: String = _

  def this(name: String) = {
    this
    this.name = name
  }

  def this(loc: Location, name: String) {
    this
    this.name = name
    this.location = loc
  }
}
