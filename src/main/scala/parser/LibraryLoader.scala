package parser

import java.io.{File, FileNotFoundException}

import compiler.ErrorHandler
import entity.Declarations
import exception.{FileException, SemanticException}

class LibraryLoader {
  var loadPath: List[String] = List(".")
  var loadingLibraries: List[String] = _
  var loadedLibraries: Map[String, Declarations] = _

  def this(loadPath: List[String]) = {
    this
    this.loadPath = loadPath
    this.loadedLibraries = Map()
    this.loadingLibraries = List()
  }

  def loadLibrary(libid:String,handler:ErrorHandler) = {
    if(loadingLibraries.contains(libid))
      throw new SemanticException(s"recursive import from ${loadingLibraries.last}: $libid")
    loadingLibraries :+= libid
    var decls = loadedLibraries(libid)
    if (decls != null) {return decls}
    decls = Parser.parseDeclFile(searchLibrary(libid),this,handler)
    loadingLibraries = loadingLibraries.dropRight(1)
    decls
  }

  def searchLibrary(libid:String) = {
    try {
      for (path <- loadPath) {
        val file = new File(s"$path/${libPath(libid)}.hb")
        if (file.exists()) return file
      }
      throw new FileNotFoundException(s"no such library header file: $libid")
    } catch {
      case ex:SecurityException =>
        throw new FileException(ex.getMessage)
    }
  }

  def libPath(str: String) = {
    str.replace('.','/')
  }

}
