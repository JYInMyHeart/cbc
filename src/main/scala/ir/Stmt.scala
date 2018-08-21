package ir

import ast.{Dumpable, Dumper, Location}

class Stmt extends Dumpable{
  var location:Location = _
  override def dump(d: Dumper): Unit = ???
}
