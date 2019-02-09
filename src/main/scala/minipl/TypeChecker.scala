package minipl

import scala.annotation.tailrec
import scala.util.Try


case class VariableSymbol(valueType: Type)

sealed trait Type

case class IntType() extends Type

case class StringType() extends Type

case class BoolType() extends Type


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
    case s@ReadOp(_) => visit(s, symbolTbl)
    case s@PrintOp(_) => visit(s, symbolTbl)
    case s@AssertOp(_) => visit(s, symbolTbl)
  }

  def visit(stmt: VariableDeclaration, symbolTbl: SymbolTable): SymbolTable = {
    if (symbolTbl.contains(stmt.name)) throw MiniPLSemanticError("Cannot redeclare variable: " + stmt.name)
    val symbolType = stmt.varType match {
      case "string" => StringType()
      case "int" => IntType()
      case "bool" => BoolType()
    }

    val newSymbolTbl = symbolTbl + (stmt.name -> VariableSymbol(symbolType))
    stmt.value match {
      case None => newSymbolTbl
      case Some(expr) => visit(expr, newSymbolTbl)
    }
  }

  def visit(stmt: VariableAssignment, symbolTbl: SymbolTable): SymbolTable = {
    if (!symbolTbl.contains(stmt.name))
      throw MiniPLSemanticError("Cannot assign to value nonexistent variable: " + stmt.name)
    val variableType = symbolTbl(stmt.name).valueType
    val exprType = visit(stmt.value, symbolTbl)
    if (exprType != variableType) throw MiniPLSemanticError("Tried to assign invalid value type to variable: " + stmt.name)
    else symbolTbl
  }

  def visit(stmt: ReadOp, symbolTbl: SymbolTable): SymbolTable = {
    val varType = visit(stmt.ref, symbolTbl)
    varType match {
      case BoolType() => throw MiniPLSemanticError("Invalid argument type for read operation")
      case _ => symbolTbl
    }
  }

  def visit(stmt: PrintOp, symbolTbl: SymbolTable): SymbolTable = {
    visit(stmt.value, symbolTbl)
    symbolTbl
  }

  def visit(stmt: AssertOp, symbolTbl: SymbolTable): SymbolTable = {
    val result = visit(stmt.expr, symbolTbl)
    result match {
      case BoolType() => symbolTbl
      case _ => throw MiniPLSemanticError("Cannot assert non-boolean value")
    }
  }

  def visit(expr: Expression, symbolTbl: SymbolTable): Type = expr match {
    case StringLiteral(_) => StringType()
    case IntLiteral(_) => IntType()
    case v@VariableRef(_) => visit(v, symbolTbl)
    case e@UnaryNot(_) => visit(e, symbolTbl)
    case e@ArithmeticExpression(_, _, _) => visit(e, symbolTbl)
    case e@BooleanExpression(_, _, _) => visit(e, symbolTbl)
  }

  def visit(expr: VariableRef, symbolTbl: SymbolTable): Type = {
    if (!symbolTbl.contains(expr.name)) throw MiniPLSemanticError("Unknown variable: " + expr.name)
    symbolTbl(expr.name).valueType
  }

  def visit(not: UnaryNot, symbolTable: SymbolTable): Type = {
    val result = visit(not.expr, symbolTable)
    result match {
      case BoolType() => BoolType()
      case _ => throw MiniPLSemanticError("The ! expression requires boolean expression")
    }
  }

  def visit(expr: ArithmeticExpression, symbolTable: SymbolTable): Type = {
    def isIntegerOp(lhs: Type, rhs: Type): Type =
      if (!lhs.isInstanceOf[IntType] || rhs != lhs)
        throw MiniPLSemanticError("Invalid type encountered in arithmetic expression")
      else IntType()

    val leftHand = visit(expr.leftHand, symbolTable)
    val rightHand = visit(expr.rightHand, symbolTable)
    expr.op match {
      case Plus() =>
        if (leftHand.isInstanceOf[StringType] || rightHand.isInstanceOf[StringType]) StringType()
        else isIntegerOp(leftHand, rightHand)
      case _ => isIntegerOp(leftHand, rightHand)
    }
  }

  def visit(expr: BooleanExpression, symbolTable: SymbolTable): Type = {
    val leftHand = visit(expr.leftHand, symbolTable)
    val rightHand = visit(expr.rightHand, symbolTable)
    expr.op match {
      case And() =>
        if (!leftHand.isInstanceOf[BoolType] || leftHand != rightHand)
          throw MiniPLSemanticError("Invalid type encountered in boolean expression")
        else BoolType()
      case _ =>
        if (leftHand != rightHand)
          throw MiniPLSemanticError("Cannot perform comparison on different types")
        else BoolType()
    }
  }
}
