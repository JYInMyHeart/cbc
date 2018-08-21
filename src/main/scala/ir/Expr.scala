package ir

import ast.Dumpable
import ctype.Ctype

abstract class Expr extends Dumpable{
  var ctype:Ctype
}
