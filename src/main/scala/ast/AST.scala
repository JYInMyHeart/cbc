package ast

import entity.{ConstantTable, Declarations, ToplevelScope}

class AST(val source:Location,
          val declarations:Declarations,
          ) extends Node{
  protected var scope:ToplevelScope = _
  protected var constantTable:ConstantTable = _

  def types() = {


  }
}
