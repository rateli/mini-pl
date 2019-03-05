package minipl.errors

final case class MiniPLSyntaxError(msg: String) extends Exception

final case class MiniPLSemanticError(msg: String) extends Exception

final case class MiniPLAssertionError() extends Exception

final case class MiniPlDivideByZeroError() extends Exception

final case class MiniPLNullPointerError(msg: String) extends Exception

final case class MiniPLRuntimeError(msg: String) extends Exception