package parser

class ParseException(var message: String) extends Exception(message) {
  var currentToken: Token = _
  var expectedTokenSequences: Array[Array[Int]] = _
  var tokenImage: Array[String] = _

  def this(currentTokenVal: Token,
           expectedTokenSequencesVal: Array[Array[Int]],
           tokenImageVal: Array[String]) = {
    this("")
    currentToken = currentTokenVal
    expectedTokenSequences = expectedTokenSequencesVal
    tokenImage = tokenImageVal
  }

  val eol: String = System.getProperty("line.separator", "\n")

  def apply(currentTokenVal: Token,
            expectedTokenSequencesVal: Array[Array[Int]],
            tokenImageVal: Array[String]
           ): ParseException = this (currentTokenVal, expectedTokenSequencesVal, tokenImageVal)


}

object ParseException {

}
