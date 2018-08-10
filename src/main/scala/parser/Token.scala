package parser

class Token extends Serializable {
  var kind:Int = _
  var image:String = _
  var beginLine:Int = _
  var beginColumn:Int = _
  var endLine:Int = _
  var endColumn:Int = _
  var next:Token = _
  var specialToken:Token = _
  def this(kind:Int,image:String) = {
    this
    this.kind = kind
    this.image = image
  }
  def this(kind:Int) = {
    this
    this.kind = kind
  }

  override def toString: String = image
}
object Token{
  def newToken(ofkind:Int,image:String) = ofkind match {
    case _ => new Token(ofkind,image)
  }

  def newToken(ofkind:Int) = new Token(ofkind)
}
