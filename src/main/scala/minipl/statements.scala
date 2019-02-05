package minipl

sealed trait Statement

case class VariableDeclaration(name: String, varType: String, value: Option[Expression]) extends Statement

case class VariableAssignment(name: String, value: Expression) extends Statement

case class ForLoop(loopVar: String, start: Expression, end: Expression, body: List[Statement]) extends Statement

case class ReadOp(name: String) extends Statement

case class PrintOp(value: Expression) extends Statement

case class AssertOp(expr: Expression) extends Statement