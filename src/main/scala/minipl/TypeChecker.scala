package minipl

import minipl.errors.MiniPLSemanticError
import minipl.utils.Symbols.SymbolTable
import minipl.utils._

import scala.util.{Failure, Success, Try}

object TypeChecker {

  def runSemanticAnalysis(program: List[Statement]): Try[SymbolTable] = {
    val symbolTbl: SymbolTable = Map.empty
    if (program.isEmpty) Success(symbolTbl)
    else
      program.foldLeft(Success(symbolTbl): Try[SymbolTable])((cur, s) => {
        if (cur.isFailure) return cur
        visit(s, cur.get) match {
          case Success(update) => Success(cur.get ++ update)
          case Failure(e) => Failure(e)
        }
      })
  }

  def visit(stmt: Statement, symbolTbl: SymbolTable): Try[SymbolTable] = stmt match {
    case s@VariableDeclaration(_, _, _) => visit(s, symbolTbl)
    case s@VariableAssignment(_, _) => visit(s, symbolTbl)
    case s@ForLoop(_, _, _, _) => visit(s, symbolTbl)
    case s@ReadOp(_) => visit(s, symbolTbl)
    case s@PrintOp(_) => visit(s, symbolTbl)
    case s@AssertOp(_) => visit(s, symbolTbl)
  }

  def visit(stmt: VariableDeclaration, symbolTbl: SymbolTable): Try[SymbolTable] = {
    if (symbolTbl.contains(stmt.name)) return Failure(MiniPLSemanticError("Cannot redeclare variable: " + stmt.name))
    val symbolType = stmt.varType match {
      case "string" => VariableSymbol(StringType(), None)
      case "int" => VariableSymbol(IntType(), None)
      case "bool" => VariableSymbol(BoolType(), None)
    }

    val newSymbolTbl = symbolTbl + (stmt.name -> symbolType)
    stmt.value match {
      case None => Success(newSymbolTbl)
      case Some(expr) => visit(expr, newSymbolTbl)
    }
  }

  def visit(stmt: VariableAssignment, symbolTbl: SymbolTable): Try[SymbolTable] = {
    if (!symbolTbl.contains(stmt.name))
      return Failure(MiniPLSemanticError("Cannot assign to value nonexistent variable: " + stmt.name))
    val varType = symbolTbl(stmt.name).varType
    visit(stmt.value, symbolTbl) match {
      case Failure(f@_) => Failure(f)
      case Success(exprType@_) =>
        if (varType == exprType) Success(symbolTbl)
        else Failure(MiniPLSemanticError("Tried to assign invalid value type to variable: " + stmt.name))
    }
  }

  def visit(stmt: ForLoop, symbolTbl: SymbolTable): Try[SymbolTable] = {
    val validLoopVar = visit(stmt.loopVar, symbolTbl) match {
      case Success(IntType()) => true
      case _ => false
    }
    if (!validLoopVar) return Failure(MiniPLSemanticError("Loop variable has to be integer type"))

    val validLoopStart = visit(stmt.start, symbolTbl) match {
      case Success(IntType()) => true
      case _ => false
    }
    if (!validLoopStart) return Failure(MiniPLSemanticError("Loop range start expression has to have integer result"))

    val validLoopEnd = visit(stmt.end, symbolTbl) match {
      case Success(IntType()) => true
      case _ => false
    }
    if (!validLoopEnd) return Failure(MiniPLSemanticError("Loop range end expression has to have integer result"))

    stmt.body.foldLeft(Success(symbolTbl): Try[SymbolTable])((cur, s) => {
      if (cur.isFailure) return cur
      visit(s, cur.get) match {
        case Success(update) => Success(cur.get ++ update)
        case Failure(e) => Failure(e)
      }
    })
  }

  def visit(stmt: ReadOp, symbolTbl: SymbolTable): Try[SymbolTable] = {
    val varType = visit(stmt.ref, symbolTbl)
    varType match {
      case Success(BoolType()) => Failure(MiniPLSemanticError("Invalid argument type for read operation"))
      case _ => Success(symbolTbl)
    }
  }

  def visit(stmt: PrintOp, symbolTbl: SymbolTable): Try[SymbolTable] = {
    visit(stmt.value, symbolTbl)
    Success(symbolTbl)
  }

  def visit(stmt: AssertOp, symbolTbl: SymbolTable): Try[SymbolTable] = {
    val result = visit(stmt.expr, symbolTbl)
    result match {
      case Success(BoolType()) => Success(symbolTbl)
      case _ => Failure(MiniPLSemanticError("Cannot assert non-boolean value"))
    }
  }

  def visit(expr: Expression, symbolTbl: SymbolTable): Try[Type] = expr match {
    case StringLiteral(_) => Success(StringType())
    case IntLiteral(_) => Success(IntType())
    case v@VariableRef(_) => visit(v, symbolTbl)
    case e@UnaryNot(_) => visit(e, symbolTbl)
    case e@ArithmeticExpression(_, _, _) => visit(e, symbolTbl)
    case e@BooleanExpression(_, _, _) => visit(e, symbolTbl)
  }

  def visit(expr: VariableRef, symbolTbl: SymbolTable): Try[Type] = {
    if (!symbolTbl.contains(expr.name)) Failure(MiniPLSemanticError("Unknown variable: " + expr.name))
    else Success(symbolTbl(expr.name).varType)
  }

  def visit(not: UnaryNot, symbolTbl: SymbolTable): Try[Type] = {
    val result = visit(not.expr, symbolTbl)
    result match {
      case Success(BoolType()) => Success(BoolType())
      case _ => Failure(MiniPLSemanticError("The ! expression requires boolean expression"))
    }
  }

  def visit(expr: ArithmeticExpression, symbolTbl: SymbolTable): Try[Type] = {
    val leftHand = visit(expr.leftHand, symbolTbl)
    val rightHand = visit(expr.rightHand, symbolTbl)
    (leftHand, rightHand) match {
      case (Success(StringType()), Success(_)) => Success(StringType())
      case (Success(_), Success(StringType())) => Success(StringType())
      case (Success(IntType()), Success(IntType())) => Success(IntType())
      case _ => Failure(MiniPLSemanticError("Invalid type encountered in arithmetic expression"))
    }
  }

  def visit(expr: BooleanExpression, symbolTbl: SymbolTable): Try[Type] = {
    val leftHand = visit(expr.leftHand, symbolTbl)
    val rightHand = visit(expr.rightHand, symbolTbl)

    if (expr.op.isInstanceOf[And]) {
      (leftHand, rightHand) match {
        case (Success(BoolType()), Success(BoolType())) => Success(IntType())
        case _ => Failure(MiniPLSemanticError("Invalid type encountered in boolean expression"))
      }
    } else {
      if (leftHand == rightHand) Success(BoolType())
      else Failure(MiniPLSemanticError("Cannot perform comparison on different types"))
    }
  }
}
