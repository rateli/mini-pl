package minipl

import scala.annotation.tailrec
import scala.util.Try


case class VariableSymbol(valueType: String, value: Option[Value])

sealed trait Value

case class IntValue(value: Int) extends Value

case class StringValue(value: String) extends Value

case class BoolValue(value: Boolean) extends Value


final case class MiniPLSemanticError(msg: String) extends Exception

object TypeChecker {

  type SymbolTable = Map[String, VariableSymbol]

  //  def runSemanticAnalysisTODO(program: List[Statement]): Try[SymbolTable] = Try(program)

  def runSemanticAnalysis(program: List[Statement]): SymbolTable = {
    val symbolTable: SymbolTable = Map.empty
    if (program.nonEmpty) visit(program, symbolTable)
    else symbolTable
  }

  @tailrec
  def visit(program: List[Statement], symbolTbl: SymbolTable): SymbolTable = program match {
    case Nil => symbolTbl
    case stmt :: rest => visit(rest, visit(stmt, symbolTbl))
  }

  def visit(stmt: Statement, symbolTbl: SymbolTable): SymbolTable = stmt match {
    case s@VariableDeclaration(_, _, _) => visit(s, symbolTbl)
    case s@VariableAssignment(_, _) => visit(s, symbolTbl)
    case s@ForLoop(_, _, _, _) => symbolTbl
    case s@ReadOp(_) => symbolTbl
    case s@PrintOp(_) => symbolTbl
    case s@AssertOp(_) => symbolTbl
  }

  def visit(stmt: VariableDeclaration, symbolTbl: SymbolTable): SymbolTable = {
    if (symbolTbl.contains(stmt.name)) throw MiniPLSemanticError("Cannot redeclare variable: " + stmt.name)
    val newSymbolTbl = symbolTbl + (stmt.name -> VariableSymbol(stmt.varType, None))
    stmt.value match {
      case None => newSymbolTbl
      case Some(expr) => visit(expr, newSymbolTbl)
    }
  }

  def visit(stmt: VariableAssignment, symbolTbl: SymbolTable): SymbolTable = {
    if (!symbolTbl.contains(stmt.name))
      throw MiniPLSemanticError("Cannot assign to value nonexistent variable: " + stmt.name)
    val variable = symbolTbl(stmt.name)
    val newSymbol = visit(stmt.value, symbolTbl) match {
      case v@StringValue(_) if variable.valueType == "string" => VariableSymbol("string", Some(v))
      case v@IntValue(_) if variable.valueType == "int" => VariableSymbol("int", Some(v))
      case v@BoolValue(_) if variable.valueType == "bool" => VariableSymbol("bool", Some(v))
      case _ => throw MiniPLSemanticError("Tried to assign invalid value type to variable: " + stmt.name)
    }
    symbolTbl + (stmt.name -> newSymbol)
  }

  def visit(expr: Expression, symbolTbl: SymbolTable): Value = expr match {
    case StringLiteral(str) => StringValue(str)
    case IntLiteral(i) => IntValue(i)
    case VariableRef(_) => BoolValue(true)
    case e@UnaryNot(_) => BoolValue(true)
    case e@ArithmeticExpression(_, _, _) => BoolValue(true)
    case e@BooleanExpression(_, _, _) => BoolValue(true)
  }
}


//case class UnaryNot(expr: Expression) extends Expression
//
//case class ArithmeticExpression(leftHand: Expression, op: String, rightHand: Expression) extends Expression
//
//case class BooleanExpression(leftHand: Expression, op: String, rightHand: Expression) extends Expression
//
//case class IntLiteral(value: Int) extends Expression
//
//case class StringLiteral(value: String) extends Expression
//
//case class VariableRef(name: String) extends Expression


/** ***************************************************************************************************************/

//case class VariableDeclaration(name: String, varType: String, value: Option[Expression]) extends Statement
//
//case class VariableAssignment(name: String, value: Expression) extends Statement
//
//case class ForLoop(loopVar: String, start: Expression, end: Expression, body: List[Statement]) extends Statement
//
//case class ReadOp(name: String) extends Statement
//
//case class PrintOp(value: Expression) extends Statement
//
//case class AssertOp(expr: Expression) extends Statement