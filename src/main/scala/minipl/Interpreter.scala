package minipl

import minipl.TypeChecker.SymbolTable

import scala.annotation.tailrec
import scala.io.StdIn

object Interpreter {

  sealed trait Value

  final case class IntValue(value: Int) extends Value

  final case class StringValue(value: String) extends Value

  final case class BoolValue(value: Boolean) extends Value

  type ValueTable = Map[String, Value]

  def runProgram(program: List[Statement], symbolTable: SymbolTable): SymbolTable = {
    run(program, symbolTable, Map.empty)
  }

  @tailrec
  def run(program: List[Statement], symbolTbl: SymbolTable, valueTbl: ValueTable): SymbolTable = program match {
    case Nil => symbolTbl
    case stmt :: rest => run(rest, visit(stmt, symbolTbl, valueTbl), valueTbl)
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

  def visit(stmt: ForLoop, symbolTbl: SymbolTable, valueTbl: ValueTable): SymbolTable = {
    val loopVarName = stmt.loopVar.name
    val start = visit(stmt.start, symbolTbl, valueTbl) match {
      case IntValue(x) => x
    }
    val end = visit(stmt.end, symbolTbl, valueTbl) match {
      case IntValue(x) => x
    }
    runLoop(start, end, stmt.body, symbolTbl, valueTbl)
  }

  def runLoop(loopVar: Int, end: Int, body: List[Statement], symbolTbl: SymbolTable, valueTbl: ValueTable): SymbolTable = {
    if (loopVar > end) symbolTbl
    else {
      val newTbl = body.foldLeft(symbolTbl)((tbl, s) => tbl ++ visit(s, tbl, valueTbl))
      runLoop(loopVar + 1, end, body, newTbl, valueTbl)
    }
  }

  def visit(stmt: ReadOp, symbolTbl: SymbolTable, valueTbl: ValueTable): SymbolTable = {
    val input = StdIn.readLine()
    val inputValue = symbolTbl(stmt.ref.name) match {
      case IntType() => IntValue(input.toInt)
      case StringType() => StringValue(input)
    }

    val newValueTbl = valueTbl + (stmt.ref.name -> inputValue)
    symbolTbl
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

  def visit(expr: ArithmeticExpression, symbolTbl: SymbolTable, valueTbl: ValueTable): Value = {
    val leftHand = visit(expr.leftHand, symbolTbl, valueTbl)
    val rightHand = visit(expr.rightHand, symbolTbl, valueTbl)
    expr.op match {
      case Plus() => plus(leftHand, rightHand)
      case op@Minus() => result(leftHand, rightHand, op)
      case op@Mul() => result(leftHand, rightHand, op)
      case op@Div() => result(leftHand, rightHand, op)
    }
  }

  def plus(lhs: Value, rhs: Value): Value = {
    if (lhs.isInstanceOf[StringType] || rhs.isInstanceOf[StringType]) concat(lhs, rhs)
    else result(lhs, rhs, Plus())
  }

  def result(lhs: Value, rhs: Value, op: Operator): IntValue = {
    val leftHand = lhs match {
      case IntValue(value) => value
    }
    val rightHand = rhs match {
      case IntValue(value) => value
    }
    op match {
      case Plus() => IntValue(leftHand + rightHand)
      case Minus() => IntValue(leftHand - rightHand)
      case Mul() => IntValue(leftHand * rightHand)
      case Div() => IntValue(leftHand / rightHand)
    }
  }

  def concat(lhs: Value, rhs: Value): StringValue = {
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
    StringValue(leftHand + rightHand)
  }

  def visit(expr: BooleanExpression, symbolTbl: SymbolTable, valueTbl: ValueTable): BoolValue = {
    val leftHand = visit(expr.leftHand, symbolTbl, valueTbl)
    val rightHand = visit(expr.rightHand, symbolTbl, valueTbl)
    expr.op match {
      case Eq() => BoolValue(leftHand == rightHand)
      case And() => and(leftHand, rightHand)
      case LT() => lessThan(leftHand, rightHand)
    }
  }

  def and(lhs: Value, rhs: Value): BoolValue = {
    val leftHand = lhs match {
      case BoolValue(value) => value
    }
    val rightHand = rhs match {
      case BoolValue(value) => value
    }
    BoolValue(leftHand && rightHand)
  }

  def lessThan(lhs: Value, rhs: Value): BoolValue = {
    val leftHand = lhs match {
      case IntValue(value) => value
    }
    val rightHand = rhs match {
      case IntValue(value) => value
    }
    BoolValue(leftHand < rightHand)
  }

}
