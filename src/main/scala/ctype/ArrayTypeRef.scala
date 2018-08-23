package ctype

class ArrayTypeRef extends TypeRef {
  var baseType :TypeRef = _
  var length:Long = _

  def this(baseType:TypeRef) = {
    this
    this.baseType = baseType
    this.length = ArrayTypeRef.undefined
  }

  def this(baseType:TypeRef,length:Long) = {
    this
    this.baseType = baseType
    this.length = length
  }

  def isArray = true

  override def equals(obj: scala.Any): Boolean =
    obj.isInstanceOf[ArrayTypeRef] && (length == obj.asInstanceOf[ArrayTypeRef].length)

  override def toString: String = baseType.toString + s"[${if(length == ArrayTypeRef.undefined) "" else length}]"
}
object ArrayTypeRef{
  val undefined = -1
}
