
import java.nio.file.{Files, Paths}

import minipl.errors._
import minipl.{Interpreter, Parser, TypeChecker}

import scala.util.{Failure, Success}

object Main {

  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("Usage: mini-pl [source file]")
      System.exit(1)
    }
    if (!Files.exists(Paths.get(args(0)))) {
      println("Given file does not exist")
      System.exit(1)
    }
    val source = scala.io.Source.fromFile(args(0)).mkString

    val result = for {
      prog <- Parser.parse(source)
      symTbl <- TypeChecker.runSemanticAnalysis(prog)
      res <- Interpreter.runProgram(prog, symTbl)
    } yield res

    result match {
      case Success(_) => System.exit(0)
      case Failure(e) => printError(e)
        System.exit(1)
    }
  }

  def printError(e: Throwable): Unit = e match {
    case MiniPLSyntaxError(msg) => println("Syntax error: " + msg)
    case MiniPLSemanticError(msg) => println("Semantic error: " + msg)
    case MiniPLRuntimeError(msg) => println("Runtime error: " + msg)
    case MiniPLAssertionError() => println("Assertion failed")
    case MiniPlDivideByZeroError() => println("Cannot divide by zero")
    case _ => println(e.toString)
  }

}
