package minipl

import scala.util.parsing.combinator._

final case class MiniPLSyntaxError(msg: String) extends Exception

sealed trait Statement

case class NoOp() extends Statement

case class VariableDeclaration(name: String, varType: String) extends Statement

case class VariableAssignment(name: String, value: Statement) extends Statement

case class VariableDeclarationWithAssignment(name: String, varType: String, value: Expression) extends Statement

sealed trait Expression

case class UnaryNot(expr: Expression) extends Expression

case class ArithmeticExpression(leftHand: Expression, op: String, rightHand: Expression) extends Expression

case class BooleanExpression(leftHand: Expression, op: String, rightHand: Expression) extends Expression

case class IntLiteral(value: Int) extends Expression

case class StringLiteral(value: String) extends Expression

case class VariableRef(value: String) extends Expression

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

  def minipl: Parser[List[Statement]] = rep1(statement)

  def statement: Parser[Statement] = (declaration ||| declarationWithAssignment ||| readOp) <~ ";"

  def declaration: Parser[Statement] =
    "var" ~> varRef ~ ":" ~ varType ^^ {
      case VariableRef(vName) ~ _ ~ vType => VariableDeclaration(vName, vType)
    }

  def declarationWithAssignment: Parser[Statement] =
    "var" ~> varRef ~ ":" ~ varType ~ ":=" ~ expr ^^ {
      case VariableRef(vName) ~ _ ~ vType ~ _ ~ vValue => VariableDeclarationWithAssignment(vName, vType, vValue)
    }

  def expr: Parser[Expression] = operand ||| unaryNot ||| binaryExpr

  def binaryExpr: Parser[Expression] = arithmeticExpr ||| booleanExpr

  def arithmeticExpr: Parser[Expression] = operand ~ """[\+\-\*\/]""".r ~ operand ^^ {
    case lhs ~ op ~ rhs => ArithmeticExpression(lhs, op, rhs)
  }

  def booleanExpr: Parser[Expression] = operand ~ """[\&\=\<]""".r ~ operand ^^ {
    case lhs ~ op ~ rhs => BooleanExpression(lhs, op, rhs)
  }

  def unaryNot: Parser[Expression] = "!" ~> expr ^^ {
    expr => UnaryNot(expr)
  }

  def subExpr: Parser[Expression] = "(" ~> expr <~ ")"

  def operand: Parser[Expression] = varRef ||| stringLiteral ||| intLiteral ||| subExpr

  def varType: Parser[String] = """(int|bool|string)""".r

  def varRef: Parser[VariableRef] =
    """[A-Za-z_][a-zA-Z0-9]*""".r ^^ {
      name => VariableRef(name)
    }

  def intLiteral: Parser[IntLiteral] =
    """[1-9][0-9]*""".r ^^ (i => i.toInt) ^^ {
      value => IntLiteral(value)
    }

  def stringLiteral: Parser[StringLiteral] =
    """\".*\"""".r ^^ {
      value => StringLiteral(value)
    }

  def readOp: Parser[Statement] = "read " ~ varRef ^^ (_ => NoOp())

  def printOp: Parser[Statement] = "print " ~ expr ^^ (_ => NoOp())

  def assertOp: Parser[Statement] = "assert " ~ expr ^^ (_ => NoOp())

}
