package entity

import ast.{Dumpable, TypeNode}

abstract class Entity extends Dumpable{
  var name:String = _
  var isPrivate:Boolean = _
  var typeNode:TypeNode = _
  var nRefered:Long = _
//  var memref:MemoryReference = _
//  var address:Operand = _

  def this(priv:Boolean,t:TypeNode,name:String) = {
    this
    this.name = name
    this.isPrivate = priv
    this.typeNode = t
    this.nRefered = 0
  }
}
