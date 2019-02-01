package minipl

import scala.util.parsing.combinator._

final case class MiniPLSyntaxError(msg: String) extends Exception

sealed trait Statement

case class NoOp() extends Statement

case class VariableDeclaration(name: String, varType: String) extends Statement

case class VariableAssignment(name: String, value: Statement) extends Statement

case class VariableDeclarationWithAssignment(name: String, varType: String, value: Statement) extends Statement

object Parser extends RegexParsers {

  override def skipWhitespace: Boolean = true

  def parse(source: String) = {
    parseAll(minipl, source) match {
      case Success(matched, _) => matched
      case Failure(msg, _) =>
        println(msg)
        throw new MiniPLSyntaxError(msg)
      case Error(msg, _) =>
        println(msg)
        throw new MiniPLSyntaxError(msg)
    }
  }

  def minipl: Parser[List[Statement]] = rep(statement)

  def statement: Parser[Statement] = (declaration ||| declarationWithAssignment ||| readCmd) <~ ";"

  def declaration: Parser[Statement] =
    "var" ~> varIdent ~ ":" ~ varType ^^ {
      case vName ~ _ ~ vType => VariableDeclaration(vName, vType)
    }

  def declarationWithAssignment: Parser[Statement] =
    "var" ~> varIdent ~ ":" ~ varType ~ ":=" ~ intLiteral ^^ {
      case vName ~ _ ~ vType ~ _ ~ value => VariableDeclarationWithAssignment(vName, vType, NoOp())
    }

  def varType: Parser[String] = """(int|bool|string)""".r

  def varIdent: Parser[String] = """[A-Za-z_][a-zA-Z0-9]*""".r

  def intLiteral: Parser[Int] =  """[1-9][0-9]*""".r ^^ (i => i.toInt)

  def readCmd: Parser[Statement] = "read" ~ varIdent ^^ (_ => NoOp())

}