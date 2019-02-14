package minipl

sealed trait Type

case class IntType() extends Type

case class StringType() extends Type

case class BoolType() extends Type