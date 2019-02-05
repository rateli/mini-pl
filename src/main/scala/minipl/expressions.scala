package minipl

sealed trait Expression

case class UnaryNot(expr: Expression) extends Expression

case class ArithmeticExpression(leftHand: Expression, op: String, rightHand: Expression) extends Expression

case class BooleanExpression(leftHand: Expression, op: String, rightHand: Expression) extends Expression

case class IntLiteral(value: Int) extends Expression

case class StringLiteral(value: String) extends Expression

case class VariableRef(name: String) extends Expression