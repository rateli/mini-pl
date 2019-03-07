package minipl

import minipl.utils._
import org.scalatest.FunSuite

import scala.util.Success

class ParserSuite extends FunSuite {

  test("Parsing variable declaration with different types") {
    val src1 = "var i : string;"
    val parseResult1 = List(VariableDeclaration("i", "string", None))
    assert(Parser.parse(src1) == Success(parseResult1))

    val src2 = "var i : int; var j : bool;"
    val parseResult2 = List(
      VariableDeclaration("i", "int", None),
      VariableDeclaration("j", "bool", None)
    )
    assert(Parser.parse(src2) == Success(parseResult2))
  }

  test("Parsing variable assignment") {
    val src1 = """i := "foo";"""
    val parseResult1 = List(VariableAssignment("i", StringLiteral("foo")))
    assert(Parser.parse(src1) == Success(parseResult1))

    val src2 = "j := 4;"
    val parseResult2 = List(VariableAssignment("j", IntLiteral(4)))
    assert(Parser.parse(src2) == Success(parseResult2))
  }

  test("Parsing variable declaration with assignment") {
    val src = """var i : string := "foo";"""
    val parseResult = List(VariableDeclaration("i", "string", Some(VariableAssignment("i", StringLiteral("foo")))))
    assert(Parser.parse(src) == Success(parseResult))
  }

  test("Parsing fails when variable assignment and declaration are wrong way around") {
    val src = """var i := "foo" : string;"""
    assert(Parser.parse(src).isFailure)
  }

  test("Parsing read operation") {
    val src = """read x;"""
    val parseResult = List(ReadOp(VariableRef("x")))
    assert(Parser.parse(src) == Success(parseResult))
  }

  test("Parsing read operation fails if not targeted for variable reference") {
    val src = """read 4;"""
    assert(Parser.parse(src).isFailure)
  }

  test("Parsing print operation") {
    val src1 = """print 4;"""
    val parseResult1 = List(PrintOp(IntLiteral(4)))
    assert(Parser.parse(src1) == Success(parseResult1))

    val src2 = """print "foo";"""
    val parseResult2 = List(PrintOp(StringLiteral("foo")))
    assert(Parser.parse(src2) == Success(parseResult2))

    val src3 = """print y;"""
    val parseResult3 = List(PrintOp(VariableRef("y")))
    assert(Parser.parse(src3) == Success(parseResult3))
  }

  test("Parsing arithmetic expressions") {
    val src1 = """print (4 - 2) + (5 * 6);"""
    val parseResult1 = List(PrintOp(
      ArithmeticExpression(
        ArithmeticExpression(IntLiteral(4), Minus(), IntLiteral(2)),
        Plus(),
        ArithmeticExpression(IntLiteral(5), Mul(), IntLiteral(6))
      )))
    assert(Parser.parse(src1) == Success(parseResult1))

    val src2 = """print "foo" + "bar";"""
    val parseResult2 = List(PrintOp(
      ArithmeticExpression(StringLiteral("foo"), Plus(), StringLiteral("bar"))
    ))
    assert(Parser.parse(src2) == Success(parseResult2))

    val src3 = """print "foo" + 5;"""
    val parseResult3 = List(PrintOp(
      ArithmeticExpression(StringLiteral("foo"), Plus(), IntLiteral(5))
    ))
    assert(Parser.parse(src3) == Success(parseResult3))

    val src4 = """i := x + 1;"""
    val parseResult4 = List(VariableAssignment(
      "i", ArithmeticExpression(VariableRef("x"), Plus(), IntLiteral(1))
    ))
    assert(Parser.parse(src4) == Success(parseResult4))
  }

  test("Parsing boolean expressions") {
    val src1 = """print 5 = x;"""
    val parseResult1 = List(PrintOp(
      BooleanExpression(IntLiteral(5), Eq(), VariableRef("x"))
    ))
    assert(Parser.parse(src1) == Success(parseResult1))

    val src2 = """print !(1 < 2);"""
    val parseResult2 = List(PrintOp(
      UnaryNot(BooleanExpression(IntLiteral(1), LT(), IntLiteral(2)))
    ))
    assert(Parser.parse(src2) == Success(parseResult2))
  }

  test("Parsing for loops") {
    val src =
      """
        for i in 1..5+1 do
          print i;
        end for;
      """
    val body = List(PrintOp(VariableRef("i")))
    val parseResult = List(ForLoop(
      VariableRef("i"),
      IntLiteral(1),
      ArithmeticExpression(IntLiteral(5), Plus(), IntLiteral(1)),
      body
    ))
    assert(Parser.parse(src) == Success(parseResult))
  }

  test("Expressions require parentheses around subexpressions") {
    val src1 = "print 5 + 4 * 5"
    assert(Parser.parse(src1).isFailure)

    val src2 = """!!(1 < 3)"""
    assert(Parser.parse(src2).isFailure)
  }

  test("Parsing statements with invalid keywords or typos fails") {
    val src1 = """vat i : string"""
    assert(Parser.parse(src1).isFailure)

    val src2 = """var i : itn"""
    assert(Parser.parse(src2).isFailure)

    val src3 = """var i : int =: 4"""
    assert(Parser.parse(src3).isFailure)

    val src4 = """readd x"""
    assert(Parser.parse(src4).isFailure)

    val src5 =
      """
        while i in 1..5+1 do
          print i;
        end for
      """
    assert(Parser.parse(src5).isFailure)
  }

  test("Parsing statements without trailing semicolon fails") {
    val src1 = """var i : string"""
    assert(Parser.parse(src1).isFailure)

    val src2 = """i := 4"""
    assert(Parser.parse(src2).isFailure)

    val src3 = """var i : int := 4"""
    assert(Parser.parse(src3).isFailure)

    val src4 = """read x"""
    assert(Parser.parse(src4).isFailure)

    val src5 =
      """
        for i in 1..5+1 do
          print i;
        end for
      """
    assert(Parser.parse(src5).isFailure)
  }

}
