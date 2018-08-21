package ctype

abstract class Ctype {

  abstract def size():Long


}
object Ctype{
  val sizeUnknown: Long = -1
}
