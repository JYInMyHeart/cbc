package ast

import parser.Token

class CflatToken(val token:Token,
                 val isSpecial:Boolean) extends Iterable[CflatToken]{

  def this(token:Token) = this(token,false)
}
