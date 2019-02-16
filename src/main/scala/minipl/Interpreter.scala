package minipl

import minipl.TypeChecker.SymbolTable

import scala.annotation.tailrec

object Interpreter {

  sealed trait Value

  final case class IntValue(value: Int) extends Value

  final case class StringValue(value: String) extends Value

  final case class BoolValue(value: Boolean) extends Value

  type ValueTable = Map[String, Value]

  def runProgram(program: List[Statement], symbolTable: SymbolTable): SymbolTable = {
    visit(program, symbolTable, Map.empty)
  }

  @tailrec
  def visit(program: List[Statement], symbolTbl: SymbolTable, valueTbl: ValueTable): SymbolTable = program match {
    case Nil => symbolTbl
    case stmt :: rest => visit(rest, visit(stmt, symbolTbl, valueTbl), valueTbl)
  }

  def visit(stmt: Statement, symbolTbl: SymbolTable, valueTbl: ValueTable): SymbolTable = stmt match {
    case s@VariableDeclaration(_, _, _) => visit(s, symbolTbl, valueTbl)
    case s@VariableAssignment(_, _) => visit(s, symbolTbl, valueTbl)
    case s@ForLoop(_, _, _, _) => visit(s, symbolTbl, valueTbl)
    case s@ReadOp(_) => visit(s, symbolTbl, valueTbl)
    case s@PrintOp(_) => visit(s, symbolTbl, valueTbl)
    case s@AssertOp(_) => visit(s, symbolTbl, valueTbl)
  }

  def visit(assertOp: AssertOp, symbolTbl: SymbolTable, valueTbl: ValueTable): SymbolTable = {
    visit(assertOp.expr, symbolTbl, valueTbl) match {
      case BoolValue(result) => if (!result) throw MiniPLAssertionError() else symbolTbl
    }
  }

  def visit(printOp: PrintOp, symbolTbl: SymbolTable, valueTbl: ValueTable): SymbolTable = {
    visit(printOp.value, symbolTbl, valueTbl) match {
      case StringValue(result) => println(result)
        symbolTbl
    }
  }

  def visit(expr: Expression, symbolTbl: SymbolTable, valueTbl: ValueTable): Value = expr match {
    case StringLiteral(value) => StringValue(value)
    case IntLiteral(value) => IntValue(value)
    case v@VariableRef(_) => visit(v, symbolTbl, valueTbl)
    case e@UnaryNot(_) => visit(e, symbolTbl, valueTbl)
    case e@ArithmeticExpression(_, _, _) => visit(e, symbolTbl, valueTbl)
    case e@BooleanExpression(_, _, _) => visit(e, symbolTbl, valueTbl)
  }

  def visit(expr: VariableRef, symbolTbl: SymbolTable, valueTbl: ValueTable): Value = {
    valueTbl.get(expr.name) match {
      case Some(value) => value
      case None => throw MiniPLNullPointerError()
    }
  }

  def visit(not: UnaryNot, symbolTbl: SymbolTable, valueTbl: ValueTable): Value =
    visit(not.expr, symbolTbl, valueTbl) match {
      case BoolValue(result) => BoolValue(!result)
    }

}
