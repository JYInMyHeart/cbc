package ast

import parser.Token

class Location(val sourceName:String,
               val token:CflatToken) {
  def this(sourceName:String,token:Token) = {
    this(sourceName,new CflatToken(token))
  }

}
