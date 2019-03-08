package minipl

import minipl.utils._
import org.scalatest.FunSuite

import scala.util.Success

class TypeCheckerSuite extends FunSuite {

  test("Type checking variable assignments") {
    val src1 = """var i : int := 100;"""
    val result1 = TypeChecker.runSemanticAnalysis(Parser.parse(src1).get)
    assert(result1.isSuccess)

    val src2 = """var i : string; i := "a"+1;"""
    val result2 = TypeChecker.runSemanticAnalysis(Parser.parse(src2).get)
    assert(result2.isSuccess)

    val src3 = """var i : bool := 1 < 0;"""
    val result3 = TypeChecker.runSemanticAnalysis(Parser.parse(src3).get)
    assert(result3.isSuccess)
  }

  test("Type checking variable assignments with invalid types") {
    val src1 = """var i : int := "foo";"""
    val result1 = TypeChecker.runSemanticAnalysis(Parser.parse(src1).get)
    assert(result1.isFailure)

    val src2 = """var i : string; i := 1+2;"""
    val result2 = TypeChecker.runSemanticAnalysis(Parser.parse(src2).get)
    assert(result2.isFailure)

    val src3 = """var i : bool := 1;"""
    val result3 = TypeChecker.runSemanticAnalysis(Parser.parse(src3).get)
    assert(result3.isFailure)
  }

  test("Type checking print statements") {
    val src1 = """print "foo";"""
    val result1 = TypeChecker.runSemanticAnalysis(Parser.parse(src1).get)
    assert(result1.isSuccess)

    val src2 = """print 1 + 2;"""
    val result2 = TypeChecker.runSemanticAnalysis(Parser.parse(src2).get)
    assert(result2.isSuccess)

    val src3 = """print !(1 = 2);"""
    val result3 = TypeChecker.runSemanticAnalysis(Parser.parse(src3).get)
    assert(result3.isSuccess)
  }

  test("Type checking read statements") {
    val src1 = """var i : int; read i;"""
    val result1 = TypeChecker.runSemanticAnalysis(Parser.parse(src1).get)
    assert(result1.isSuccess)

    val src2 = """var i : string; read i;"""
    val result2 = TypeChecker.runSemanticAnalysis(Parser.parse(src2).get)
    assert(result2.isSuccess)

    val src3 = """var i : bool; read i;"""
    val result3 = TypeChecker.runSemanticAnalysis(Parser.parse(src3).get)
    assert(result3.isFailure)
  }

  test("Type checking loop declarations") {
    val src1 =
      """var i: int;
        |for i in 1..5 do
        |  print i;
        |end for;""".stripMargin
    val result1 = TypeChecker.runSemanticAnalysis(Parser.parse(src1).get)
    assert(result1.isSuccess)

    val src2 =
      """var i : int;
        |var j : string;
        |for i in 1..j do
        |  print i;
        |end for;""".stripMargin
    val result2 = TypeChecker.runSemanticAnalysis(Parser.parse(src2).get)
    assert(result2.isFailure)

    val src3 =
      """var i : int;
        |for i in 1..(1 < 2) do
        |  print i;
        |end for;""".stripMargin
    val result3 = TypeChecker.runSemanticAnalysis(Parser.parse(src3).get)
    assert(result3.isFailure)

    val src4 =
      """var i : string;
        |for i in 1..5 do
        |  print i;
        |end for;""".stripMargin
    val result4 = TypeChecker.runSemanticAnalysis(Parser.parse(src4).get)
    assert(result4.isFailure)
  }

  test("Type checking arithmetic expressions") {
    val src1 = """var i : int; var j: string; var ij : string := i+j;"""
    val result1 = TypeChecker.runSemanticAnalysis(Parser.parse(src1).get)
    assert(result1.isSuccess)

    val src2 = """var i : int := 0; var j: string := "b"; var ij : string := i-j;"""
    val result2 = TypeChecker.runSemanticAnalysis(Parser.parse(src2).get)
    assert(result2.isFailure)

    val src3 = """var i : int; var j: bool; var ij : string := i+j;"""
    val result3 = TypeChecker.runSemanticAnalysis(Parser.parse(src3).get)
    assert(result3.isFailure)

    val src4 = """var i : string; var j: bool; var ij : string := i+j;"""
    val result4 = TypeChecker.runSemanticAnalysis(Parser.parse(src4).get)
    assert(result4.isSuccess)
  }

  test("Type checking boolean expressions") {
    val src1 = """var i : int; var j: bool := i < 5;"""
    val result1 = TypeChecker.runSemanticAnalysis(Parser.parse(src1).get)
    assert(result1.isSuccess)

    val src2 = """var i : int := 0; var j: bool := !i;"""
    val result2 = TypeChecker.runSemanticAnalysis(Parser.parse(src2).get)
    assert(result2.isFailure)

    val src3 = """var i : int; var j: string; var ij : bool := i < j;"""
    val result3 = TypeChecker.runSemanticAnalysis(Parser.parse(src3).get)
    assert(result3.isFailure)

    val src4 = """var i : int; var j: bool := !(i = 7);"""
    val result4 = TypeChecker.runSemanticAnalysis(Parser.parse(src4).get)
    assert(result4.isSuccess)

    val src5 = """var i : string := "foo"; var j: bool := !i;"""
    val result5 = TypeChecker.runSemanticAnalysis(Parser.parse(src5).get)
    assert(result5.isFailure)
  }

  test("Type checker produces correct symbol table") {
    val src1 =
      """
        |var i : int;
        |var j : string := "foo";
        |var k : bool;
      """.stripMargin
    val result1 = TypeChecker.runSemanticAnalysis(Parser.parse(src1).get)
    assert(result1.isSuccess)
    assert(result1 == Success(Map(
      "i" -> VariableSymbol(IntType(), None),
      "j" -> VariableSymbol(StringType(), None),
      "k" -> VariableSymbol(BoolType(), None),
    )))
  }
}
