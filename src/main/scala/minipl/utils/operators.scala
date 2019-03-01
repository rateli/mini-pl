package minipl.utils

sealed trait Operator

// "+" : (int, int) -> int, (string, string) -> string
case class Plus() extends Operator

// "-" : (int, int) -> int
case class Minus() extends Operator

// "*" : int, int) -> int
case class Mul() extends Operator

// "/" : (int, int) -> int
case class Div() extends Operator

// "&" : (bool, bool) -> bool
case class And() extends Operator

// "=" : (T, T) -> bool
case class Eq() extends Operator

// "<" : (T, T) -> bool
case class LT() extends Operator