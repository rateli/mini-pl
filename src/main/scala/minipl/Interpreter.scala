package minipl

import minipl.TypeChecker.SymbolTable

import scala.annotation.tailrec

sealed trait Value

final case class IntValue(value: Int) extends Value

final case class StringValue(value: String) extends Value

final case class BoolValue(value: Boolean) extends Value

object Interpreter {

  def runProgram(program: List[Statement], symbolTable: SymbolTable): SymbolTable = {
    visit(program, symbolTable)
  }

  @tailrec
  def visit(program: List[Statement], symbolTbl: SymbolTable): SymbolTable = program match {
    case Nil => symbolTbl
    case stmt :: rest => visit(rest, visit(stmt, symbolTbl))
  }

  def visit(stmt: Statement, symbolTbl: SymbolTable): SymbolTable = stmt match {
    case s@VariableDeclaration(_, _, _) => visit(s, symbolTbl)
    case s@VariableAssignment(_, _) => visit(s, symbolTbl)
    case s@ForLoop(_, _, _, _) => visit(s, symbolTbl)
    case s@ReadOp(_) => visit(s, symbolTbl)
    case s@PrintOp(_) => visit(s, symbolTbl)
    case s@AssertOp(_) => visit(s, symbolTbl)
  }

  def visit(assertOp: AssertOp, symbolTable: SymbolTable): SymbolTable = {
    visit(assertOp.expr, symbolTable) match {
      case BoolValue(result) => if (!result) throw MiniPLAssertionError() else symbolTable
    }
  }

  def visit(printOp: PrintOp, symbolTable: SymbolTable): SymbolTable = {
    visit(printOp.value, symbolTable) match {
      case StringValue(result) => println(result)
        symbolTable
    }
  }

  def visit(expr: Expression, symbolTbl: SymbolTable): Value = expr match {
    case StringLiteral(value) => StringValue(value)
    case IntLiteral(value) => IntValue(value)
    case v@VariableRef(_) => visit(v, symbolTbl)
    case e@UnaryNot(_) => visit(e, symbolTbl)
    case e@ArithmeticExpression(_, _, _) => visit(e, symbolTbl)
    case e@BooleanExpression(_, _, _) => visit(e, symbolTbl)
  }
}
