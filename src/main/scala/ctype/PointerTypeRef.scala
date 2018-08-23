package ctype

class PointerTypeRef extends TypeRef {
    var baseType:TypeRef = _

  def this(ref:TypeRef) = {
    this
    this.location = ref.location
    this.baseType = ref
  }

  def isPointer = true

  override def equals(obj: scala.Any): Boolean =
    obj.isInstanceOf[PointerTypeRef] && (baseType == obj.asInstanceOf[PointerTypeRef].baseType)

  override def toString: String = baseType.toString + "*"
}
