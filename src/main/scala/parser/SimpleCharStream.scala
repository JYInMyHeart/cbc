package parser

import java.io.{IOException, Reader}

class SimpleCharStream {
  val staticFlag = false
  var bufSize = 0
  var available = 0
  var tokenBegin = 0

  var bufPos = -1
  var bufLine: Array[Int] = _
  var bufColumn: Array[Int] = _
  var column = 0
  var line = 1
  var prevCharIsCR = false
  var prevCharIsLF = false

  var inputStream: Reader = _

  var buffer: Array[Char] = _
  var maxNextCharInd = 0
  var inBuf = 0
  var tabSize = 0


  def beginToken(): Char = {
    tokenBegin = -1
    val c = readChar()
    tokenBegin = bufPos
    c
  }

  def updateLineColumn(c: Char) = {
    column += 1
    if (prevCharIsLF) {
      prevCharIsLF = false
      column = 1
      line += column
    } else if (prevCharIsCR) {
      prevCharIsCR = false
      if (c == '\n')
        prevCharIsLF = true
      else {
        column = 1
        line += column
      }
    }

    c match {
      case '\r' =>
        prevCharIsCR = true
      case '\n' =>
        prevCharIsLF = true
      case '\t' =>
        column -= 1
        column += (tabSize - (column % tabSize))
      case _ =>
    }

    bufLine(bufPos) = line
    bufColumn(bufPos) = column
  }

  def readChar(): Char = {
    inBuf match {
      case x if x > 0 =>
        inBuf -= 1
        if (bufPos + 1 == bufSize)
          bufPos = 0
        return buffer(bufPos)
      case _ =>
    }
    if (bufPos + 1 >= maxNextCharInd)
      fillBuff()
    val c = buffer(bufPos)
    updateLineColumn(c)
    c
  }


  def backup(i: Int) = {
    inBuf += i
    bufPos -= i
    if(bufPos < 0)
      bufPos += bufSize
  }

  def fillBuff() = {
    if (maxNextCharInd == available) {
      if (available == bufSize) {
        if (tokenBegin > 2048) {
          maxNextCharInd = 0
          bufPos = 0
          available = tokenBegin
        } else if (tokenBegin < 0) {
          maxNextCharInd = 0
          bufPos = maxNextCharInd
        } else
          expandBuff(false)
      } else if (available > tokenBegin)
        available = bufSize
      else if ((tokenBegin - available) < 2048)
        expandBuff(true)
      else
        available = tokenBegin
    }
    var i = 0
    try {
      i = inputStream.read(buffer, maxNextCharInd, available - maxNextCharInd)
      if (i == -1) {
        inputStream.close()
        throw new IOException()
      } else
        maxNextCharInd += i
    } catch {
      case e: IOException =>
        bufPos -= 1
        backup(0)
        if (tokenBegin == -1)
          tokenBegin = bufPos
        throw e
    }
  }

  def expandBuff(bool: Boolean): Unit = {
    val newBuffer = Array.ofDim[Char](bufSize + 2048)
    val newBufLine = Array.ofDim[Int](bufSize + 2048)
    val newBufColumn = Array.ofDim[Int](bufSize + 2048)

    try {
      if (bool) {

        Array.copy(buffer, tokenBegin, newBuffer, 0, bufSize - tokenBegin)
        Array.copy(buffer, 0, newBuffer, bufSize - tokenBegin, bufPos)
        buffer = newBuffer

        Array.copy(bufLine, tokenBegin, newBufLine, 0, bufSize - tokenBegin)
        Array.copy(bufLine, 0, newBufLine, bufSize - tokenBegin, bufPos)
        bufLine = newBufLine

        Array.copy(bufColumn, tokenBegin, newBufColumn, 0, bufSize - tokenBegin)
        Array.copy(bufColumn, 0, newBufColumn, bufSize - tokenBegin, bufPos)
        bufColumn = newBufColumn

        bufPos += (bufSize - tokenBegin)
        maxNextCharInd = bufPos
      } else {
        Array.copy(buffer, tokenBegin, newBuffer, 0, bufSize - tokenBegin)
        buffer = newBuffer

        Array.copy(bufLine, tokenBegin, newBufLine, 0, bufSize - tokenBegin)
        bufLine = newBufLine

        Array.copy(bufColumn, tokenBegin, newBufColumn, 0, bufSize - tokenBegin)
        bufColumn = newBufColumn

        bufPos -= tokenBegin
        maxNextCharInd = bufPos
      }
    } catch {
      case e: Throwable =>
        throw new Error(e.getMessage)
    }

    bufSize += 2048
    available = bufSize
    tokenBegin = 0
  }



}
