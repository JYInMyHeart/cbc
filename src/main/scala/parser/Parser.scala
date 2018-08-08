package parser

import java.io.File

import compiler.ErrorHandler

class Parser{

}
object Parser {

  def parseFile(file:File,loader:LibraryLoader, errorHandler:ErrorHandler) = {
    parseFile(file,loader,errorHandler,debug).parse()
  }
}
