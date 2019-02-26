package minipl

import minipl.TypeChecker.SymbolTable

import scala.annotation.tailrec
import scala.io.StdIn

object Interpreter {

  trait Value

  final case class IntValue(value: Int) extends Value

  final case class StringValue(value: String) extends Value

  final case class BoolValue(value: Boolean) extends Value

  type ValueTable = Map[String, Value]

  def runProgram(program: List[Statement], symbolTable: SymbolTable): SymbolTable = {
    run(program, symbolTable)
  }

  @tailrec
  def run(program: List[Statement], symbolTbl: SymbolTable): SymbolTable = program match {
    case Nil => symbolTbl
    case stmt :: rest => run(rest, visit(stmt, symbolTbl))
  }

  def visit(stmt: Statement, symbolTbl: SymbolTable): SymbolTable = stmt match {
    case s@VariableDeclaration(_, _, _) => visit(s, symbolTbl)
    case s@VariableAssignment(_, _) => visit(s, symbolTbl)
    case s@ForLoop(_, _, _, _) => visit(s, symbolTbl)
    case s@ReadOp(_) => visit(s, symbolTbl)
    case s@PrintOp(_) => visit(s, symbolTbl)
    case s@AssertOp(_) => visit(s, symbolTbl)
  }

  def visit(stmt: VariableDeclaration, symbolTbl: SymbolTable): SymbolTable = stmt.value match {
    case None => symbolTbl
    case Some(expr) => visit(expr, symbolTbl)
  }

  def visit(stmt: VariableAssignment, symbolTbl: SymbolTable): SymbolTable = {
    val variable = symbolTbl(stmt.name)
    val newValue = visit(stmt.value, symbolTbl)
    symbolTbl + (stmt.name -> VariableSymbol(variable.varType, Some(newValue)))
  }

  def visit(assertOp: AssertOp, symbolTbl: SymbolTable): SymbolTable = {
    visit(assertOp.expr, symbolTbl) match {
      case BoolValue(result) => if (!result) throw MiniPLAssertionError() else symbolTbl
    }
  }

  def visit(stmt: ForLoop, symbolTbl: SymbolTable): SymbolTable = {
    val loopVarName = stmt.loopVar.name
    val start = visit(stmt.start, symbolTbl) match {
      case IntValue(x) => x
    }
    val end = visit(stmt.end, symbolTbl) match {
      case IntValue(x) => x
    }
    runLoop(start, end, stmt.body, symbolTbl)
  }

  def runLoop(loopVar: Int, end: Int, body: List[Statement], symbolTbl: SymbolTable): SymbolTable = {
    if (loopVar > end) symbolTbl
    else {
      val newTbl = body.foldLeft(symbolTbl)((tbl, s) => tbl ++ visit(s, tbl))
      runLoop(loopVar + 1, end, body, newTbl)
    }
  }

  def visit(stmt: ReadOp, symbolTbl: SymbolTable): SymbolTable = {
    val input = StdIn.readLine()
    val inputValue = symbolTbl(stmt.ref.name).varType match {
      case t@IntType() => VariableSymbol(t, Some(IntValue(input.toInt)))
      case t@StringType() => VariableSymbol(t, Some(StringValue(input)))
    }
    symbolTbl + (stmt.ref.name -> inputValue)
  }

  def visit(printOp: PrintOp, symbolTbl: SymbolTable): SymbolTable = {
    visit(printOp.value, symbolTbl) match {
      case StringValue(result) => println(result)
    }
    symbolTbl
  }

  def visit(expr: Expression, symbolTbl: SymbolTable): Value = expr match {
    case StringLiteral(value) => StringValue(value)
    case IntLiteral(value) => IntValue(value)
    case v@VariableRef(_) => visit(v, symbolTbl)
    case e@UnaryNot(_) => visit(e, symbolTbl)
    case e@ArithmeticExpression(_, _, _) => visit(e, symbolTbl)
    case e@BooleanExpression(_, _, _) => visit(e, symbolTbl)
  }

  def visit(expr: VariableRef, symbolTbl: SymbolTable): Value = {
    val z = symbolTbl.get(expr.name)

    if (!symbolTbl.contains(expr.name)) throw MiniPLSyntaxError("foo bar")
    symbolTbl.get(expr.name) match {
      case Some(VariableSymbol(_, Some(value))) => value
      //      case _ => throw MiniPLNullPointerError()
      case _ => throw MiniPLSyntaxError("bar baz")
    }
  }

  def visit(not: UnaryNot, symbolTbl: SymbolTable): Value =
    visit(not.expr, symbolTbl) match {
      case BoolValue(result) => BoolValue(!result)
    }

  def visit(expr: ArithmeticExpression, symbolTbl: SymbolTable): Value = {
    val leftHand = visit(expr.leftHand, symbolTbl)
    val rightHand = visit(expr.rightHand, symbolTbl)
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

  def visit(expr: BooleanExpression, symbolTbl: SymbolTable): BoolValue = {
    val leftHand = visit(expr.leftHand, symbolTbl)
    val rightHand = visit(expr.rightHand, symbolTbl)
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
