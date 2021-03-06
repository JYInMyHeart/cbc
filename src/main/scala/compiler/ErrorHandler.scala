package compiler

import java.io.{OutputStream, PrintStream}

import ast.Location

class ErrorHandler(val programId:String,
                   val stream:PrintStream,
                   var nError:Long,
                   var nWarning:Long) {
  def this(progid:String) = {
    this(progid,System.err,0,0)
  }

  def this(progid:String,stream:OutputStream) = {
    this(progid,new PrintStream(stream),0,0)
  }

  def error(loc:Location,msg:String): Unit = {
    error(s"${loc.toString}:$msg")
  }

  def error(msg:String): Unit = {
    stream.println(s"$programId: error: $msg")
    nError += 1
  }

  def warn(loc:Location,msg:String): Unit = {
    error(s"${loc.toString}:$msg")
  }

  def warn(msg:String): Unit = {
    stream.println(s"$programId: warning: $msg")
    nError += 1
  }

  def errorOccured() = nError > 0
}
