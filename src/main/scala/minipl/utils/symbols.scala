package minipl.utils

sealed trait Type

case class IntType() extends Type

case class StringType() extends Type

case class BoolType() extends Type


sealed trait Value

final case class IntValue(value: Int) extends Value

final case class StringValue(value: String) extends Value

final case class BoolValue(value: Boolean) extends Value


final case class VariableSymbol(varType: Type, value: Option[Value])

object Symbols {
  type SymbolTable = Map[String, VariableSymbol]
}
