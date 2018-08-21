package ast

import ctype.Ctype

class Slot extends Node{
  var typeNode:TypeNode = _
  var name:String =  _
  var offset:Long = _

  def this(t:TypeNode,n:String) = {
    this
    typeNode = t
    name = n
    offset = Ctype.sizeUnknown
  }

  override def location(): Location = ???

  override def dump(d: Dumper): Unit = ???
}
