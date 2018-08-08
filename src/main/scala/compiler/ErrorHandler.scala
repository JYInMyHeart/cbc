package compiler

import java.io.PrintStream

import ast.Location

import scala.sys.process.processInternal.OutputStream

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

  def error(loc:Location,msg:String) = {
    error(s"${loc.toString}:$msg")
  }

  def error(msg:String) = {
    stream.println(s"$programId: error: $msg")
    nError += 1
  }

  def warn(loc:Location,msg:String) = {
    error(s"${loc.toString}:$msg")
  }

  def warn(msg:String) = {
    stream.println(s"$programId: warning: $msg")
    nError += 1
  }

  def errorOccured() = nError > 0
}
