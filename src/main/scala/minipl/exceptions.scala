package minipl

final case class MiniPLSyntaxError(msg: String) extends Exception

final case class MiniPLSemanticError(msg: String) extends Exception

final case class MiniPLAssertionError() extends Exception