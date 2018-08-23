package ctype

import ast.Location

class IntegerTypeRef extends TypeRef {
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
object IntegerTypeRef{
  def charRef(loc: Location): IntegerTypeRef = {
    new IntegerTypeRef(loc, "char")
  }

  def shortRef(loc: Location): IntegerTypeRef = {
    new IntegerTypeRef(loc, "short")
  }

  def intRef(loc: Location): IntegerTypeRef = {
    new IntegerTypeRef(loc, "int")
  }

  def longRef(loc: Location): IntegerTypeRef = {
    new IntegerTypeRef(loc, "long")
  }

  def ucharRef(loc: Location): IntegerTypeRef = {
    new IntegerTypeRef(loc, "unsigned int")
  }

  def ushortRef(loc: Location): IntegerTypeRef = {
    new IntegerTypeRef(loc, "unsigned short")
  }

  def uintRef(loc: Location): IntegerTypeRef = {
    new IntegerTypeRef(loc, "unsigned int")
  }

  def ulongRef(loc: Location): IntegerTypeRef = {
    new IntegerTypeRef(loc, "unsigned long")
  }

  def charRef(): IntegerTypeRef = {
    new IntegerTypeRef("char")
  }

  def shortRef(): IntegerTypeRef = {
    new IntegerTypeRef("short")
  }

  def intRef(): IntegerTypeRef = {
    new IntegerTypeRef("int")
  }

  def longRef(): IntegerTypeRef = {
    new IntegerTypeRef("long")
  }

  def ucharRef(): IntegerTypeRef = {
    new IntegerTypeRef("unsigned int")
  }

  def ushortRef(): IntegerTypeRef = {
    new IntegerTypeRef("unsigned short")
  }

  def uintRef(): IntegerTypeRef = {
    new IntegerTypeRef("unsigned int")
  }

  def ulongRef(): IntegerTypeRef = {
    new IntegerTypeRef("unsigned long")
  }
}
