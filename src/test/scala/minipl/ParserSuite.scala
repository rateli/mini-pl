package minipl

import minipl.utils._
import minipl.errors._
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
    val parseResult1 = List(VariableAssignment("i", StringLiteral("foobar")))
    assert(Parser.parse(src1) == Success(parseResult1))

    val src2 = "j := 4;"
    val parseResult2 = List(VariableAssignment("i", IntLiteral(4)))
    assert(Parser.parse(src2) == Success(parseResult2))
  }

  test("Parsing variable declaration with assignment") {
    val src = """var i : string := "foo";"""
    val parseResult = List(VariableDeclaration("i", "string", Some(VariableAssignment("i", StringLiteral("foo")))))
    assert(Parser.parse(src) == Success(parseResult))
  }


}
