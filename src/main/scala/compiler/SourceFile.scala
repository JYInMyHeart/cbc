package compiler

class SourceFile(val name:String) extends LdArg {
  private val originalName:String = name
  private var currentName = name
  override def isSourceFile(): Boolean = true
  override def toString: String = currentName
  def path = currentName
  def setCurrentName(name:String) = currentName = name

}

object SourceFile {
  val EXT_CFLAT_SOURCE = ".cb"
  val EXT_ASSEMBLY_SOURCE = ".s"
  val EXT_OBJECT_FILE = ".o"
  val EXT_STATIC_LIBRARY = ".a"
  val EXT_SHARED_LIBRARY = ".so"
  val EXT_EXECUTABLE_FILE = ""

  val KNOWN_EXTENSIONS = Array(
    EXT_CFLAT_SOURCE,
    EXT_ASSEMBLY_SOURCE,
    EXT_OBJECT_FILE,
    EXT_STATIC_LIBRARY,
    EXT_SHARED_LIBRARY,
    EXT_EXECUTABLE_FILE
  )
}
