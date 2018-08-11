package parser

trait ParserConstants {
  /** End of File. */
  val EOF = 0
  /** RegularExpression Id. */
  val SPACES = 1
  val BLOCK_COMMENT = 4
  val LINE_COMMENT = 5
  val VOID = 6
  val CHAR = 7
  val SHORT = 8
  val INT = 9
  val LONG = 10
  val STRUCT = 11
  val UNION = 12
  val ENUM = 13
  val STATIC = 14
  val EXTERN = 15
  val CONST = 16
  val SIGNED = 17
  val UNSIGNED = 18
  val IF = 19
  val ELSE = 20
  val SWITCH = 21
  val CASE = 22
  val DEFAULT_ = 23
  val WHILE = 24
  val DO = 25
  val FOR = 26
  val RETURN = 27
  val BREAK = 28
  val CONTINUE = 29
  val GOTO = 30
  val TYPEDEF = 31
  val IMPORT = 32
  val SIZEOF = 33
  val IDENTIFIER = 34
  val INTEGER = 35
  val CHARACTER = 40
  val STRING = 45

  /** Lexical state. */
  val DEFAULT = 0
  val IN_BLOCK_COMMENT = 1
  val IN_CHARACTER = 2
  val CHARACTER_TERM = 3
  val IN_STRING = 4


  /** Literal token values. */
  val tokenImage = Array("<EOF>", "<SPACES>", "\"/*\"", "<token of kind 3>", "\"*/\"", "<LINE_COMMENT>", "\"void\"", "\"char\"", "\"short\"", "\"int\"", "\"long\"", "\"struct\"", "\"union\"", "\"enum\"", "\"static\"", "\"extern\"", "\"const\"", "\"signed\"", "\"unsigned\"", "\"if\"", "\"else\"", "\"switch\"", "\"case\"", "\"default\"", "\"while\"", "\"do\"", "\"for\"", "\"return\"", "\"break\"", "\"continue\"", "\"goto\"", "\"typedef\"", "\"import\"", "\"sizeof\"", "<IDENTIFIER>", "<INTEGER>", "\"\\\'\"", "<token of kind 37>", "<token of kind 38>", "<token of kind 39>", "\"\\\'\"", "\"\\\"\"", "<token of kind 42>", "<token of kind 43>", "<token of kind 44>", "\"\\\"\"", "\"(\"", "\".\"", "\";\"", "\"=\"", "\",\"", "\")\"", "\"...\"", "\"{\"", "\"}\"", "\"[\"", "\"]\"", "\"*\"", "\":\"", "\"+=\"", "\"-=\"", "\"*=\"", "\"/=\"", "\"%=\"", "\"&=\"", "\"|=\"", "\"^=\"", "\"<<=\"", "\">>=\"", "\"?\"", "\"||\"", "\"&&\"", "\">\"", "\"<\"", "\">=\"", "\"<=\"", "\"==\"", "\"!=\"", "\"|\"", "\"^\"", "\"&\"", "\">>\"", "\"<<\"", "\"+\"", "\"-\"", "\"/\"", "\"%\"", "\"++\"", "\"--\"", "\"!\"", "\"~\"", "\"->\"")

}
