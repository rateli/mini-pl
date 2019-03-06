package minipl

import minipl.utils.Symbols.SymbolTable
import minipl.utils._
import minipl.errors._

import scala.util.{Failure, Success, Try}

object Interpreter {

  def runProgram(program: List[Statement], symbolTbl: SymbolTable): Try[SymbolTable] = {
    if (program.isEmpty) Failure(MiniPLRuntimeError("No program to run"))
    else {
      program.foldLeft(Success(symbolTbl): Try[SymbolTable])((cur, s) => {
        if (cur.isFailure) return cur
        visit(s, cur.get) match {
          case Success(update) => Success(cur.get ++ update)
          case Failure(e) => Failure(e)
        }
      })
    }
  }

  def visit(stmt: Statement, symbolTbl: SymbolTable): Try[SymbolTable] = stmt match {
    case s@VariableDeclaration(_, _, _) => visit(s, symbolTbl)
    case s@VariableAssignment(_, _) => visit(s, symbolTbl)
    case s@ForLoop(_, _, _, _) => visit(s, symbolTbl)
    case s@ReadOp(_) => visit(s, symbolTbl)
    case s@PrintOp(_) => visit(s, symbolTbl)
    case s@AssertOp(_) => visit(s, symbolTbl)
  }

  def visit(stmt: VariableDeclaration, symbolTbl: SymbolTable): Try[SymbolTable] = stmt.value match {
    case None => Success(symbolTbl)
    case Some(expr) => visit(expr, symbolTbl)
  }

  def visit(stmt: VariableAssignment, symbolTbl: SymbolTable): Try[SymbolTable] = {
    val variable = symbolTbl(stmt.name)
    visit(stmt.value, symbolTbl) match {
      case Failure(e) => Failure(e)
      case Success(newVal@_) => Success(symbolTbl + (stmt.name -> VariableSymbol(variable.varType, Some(newVal))))
    }
  }

  def visit(assertOp: AssertOp, symbolTbl: SymbolTable): Try[SymbolTable] = {
    visit(assertOp.expr, symbolTbl) match {
      case Success(BoolValue(result)) =>
        if (!result) Failure(MiniPLAssertionError()) else Success(symbolTbl)
    }
  }

  def visit(stmt: ForLoop, symbolTbl: SymbolTable): Try[SymbolTable] = {
    val name = stmt.loopVar.name
    val loopStart = visit(stmt.start, symbolTbl)
    val loopEnd = visit(stmt.end, symbolTbl)

    (loopStart, loopEnd) match {
      case (Success(IntValue(start)), Success(IntValue(end))) => iterate(name, start, end, stmt.body, Success(symbolTbl))
      case _ => Failure(MiniPLRuntimeError("Failed interpreting loop"))
    }
  }

  def iterate(loopVarName: String, loopVar: Int, end: Int, body: List[Statement], symbolTbl: Try[SymbolTable]): Try[SymbolTable] = {
    if (symbolTbl.isFailure) symbolTbl
    else if (loopVar > end) symbolTbl
    else {
      val symTbl = symbolTbl.get + (loopVarName -> VariableSymbol(IntType(), Some(IntValue(loopVar))))

      val res = body.foldLeft(Success(symTbl): Try[SymbolTable])((curST, s) => {
        if (curST.isFailure) return curST
        visit(s, curST.get) match {
          case Success(newST) => Success(curST.get ++ newST)
          case Failure(e) => Failure(e)
        }
      })
      iterate(loopVarName, loopVar + 1, end, body, res)
    }
  }

  def visit(stmt: ReadOp, symbolTbl: SymbolTable): Try[SymbolTable] = {
    val input = scala.io.StdIn.readLine()
    val inputValue = symbolTbl(stmt.ref.name).varType match {
      case t@IntType() => VariableSymbol(t, Some(IntValue(input.toInt)))
      case t@StringType() => VariableSymbol(t, Some(StringValue(input)))
    }
    Success(symbolTbl + (stmt.ref.name -> inputValue))
  }

  def visit(printOp: PrintOp, symbolTbl: SymbolTable): Try[SymbolTable] = {
    visit(printOp.value, symbolTbl) match {
      case Success(StringValue(v)) => print(v)
      case Success(IntValue(v)) => print(v)
      case Success(BoolValue(v)) => print(v)
    }
    Success(symbolTbl)
  }

  def visit(expr: Expression, symbolTbl: SymbolTable): Try[Value] = expr match {
    case StringLiteral(value) => Success(StringValue(value))
    case IntLiteral(value) => Success(IntValue(value))
    case v@VariableRef(_) => visit(v, symbolTbl)
    case e@UnaryNot(_) => visit(e, symbolTbl)
    case e@ArithmeticExpression(_, _, _) => visit(e, symbolTbl)
    case e@BooleanExpression(_, _, _) => visit(e, symbolTbl)
  }

  def visit(expr: VariableRef, symbolTbl: SymbolTable): Try[Value] = {
    symbolTbl.get(expr.name) match {
      case Some(VariableSymbol(_, Some(value))) => Success(value)
      case _ => Failure(MiniPLNullPointerError("Referenced null variable: " + expr.name))
    }
  }

  def visit(not: UnaryNot, symbolTbl: SymbolTable): Try[Value] =
    visit(not.expr, symbolTbl) match {
      case Success(BoolValue(result)) => Success(BoolValue(!result))
    }

  def visit(expr: ArithmeticExpression, symbolTbl: SymbolTable): Try[Value] = {
    val lhs = visit(expr.leftHand, symbolTbl)
    val rhs = visit(expr.rightHand, symbolTbl)
    (lhs, expr.op, rhs) match {
      case (Failure(e), _, _) => Failure(e)
      case (_, _, Failure(e)) => Failure(e)
      case (Success(l@_), op@_, Success(r@_)) => result(l, r, op)
    }
  }

  def result(lhs: Value, rhs: Value, op: Operator): Try[Value] = {
    (lhs, op, rhs) match {
      case (StringValue(_), Plus(), _) => concat(lhs, rhs)
      case (_, Plus(), StringValue(_)) => concat(lhs, rhs)
      case (IntValue(l@_), Plus(), IntValue(r@_)) => Success(IntValue(l + r))
      case (IntValue(l@_), Minus(), IntValue(r@_)) => Success(IntValue(l - r))
      case (IntValue(l@_), Mul(), IntValue(r@_)) => Success(IntValue(l * r))
      case (IntValue(_), Div(), IntValue(0)) => Failure(MiniPlDivideByZeroError())
      case (IntValue(l@_), Div(), IntValue(r@_)) => Success(IntValue(l / r))
    }
  }

  def concat(lhs: Value, rhs: Value): Try[StringValue] = {
    val leftHand = lhs match {
      case StringValue(value) => value
      case IntValue(value) => value.toString
      case BoolValue(value) => value.toString
    }
    val rightHand = rhs match {
      case StringValue(value) => value
      case IntValue(value) => value.toString
      case BoolValue(value) => value.toString
    }
    Success(StringValue(leftHand + rightHand))
  }

  def visit(expr: BooleanExpression, symbolTbl: SymbolTable): Try[BoolValue] = {
    val leftHand = visit(expr.leftHand, symbolTbl)
    val rightHand = visit(expr.rightHand, symbolTbl)
    (leftHand, expr.op, rightHand) match {
      case (Failure(e), _, _) => Failure(e)
      case (_, _, Failure(e)) => Failure(e)
      case (Success(lhs@_), Eq(), Success(rhs@_)) => Success(BoolValue(lhs == rhs))
      case (Success(lhs@_), And(), Success(rhs@_)) => and(lhs, rhs)
      case (Success(lhs@_), LT(), Success(rhs@_)) => lessThan(lhs, rhs)
    }
  }

  def and(lhs: Value, rhs: Value): Try[BoolValue] =
    (lhs, rhs) match {
      case (BoolValue(l), BoolValue(r)) => Success(BoolValue(l && r))
    }


  def lessThan(lhs: Value, rhs: Value): Try[BoolValue] = {
    (lhs, rhs) match {
      case (IntValue(l), IntValue(r)) => Success(BoolValue(l <= r))
    }
  }

}
