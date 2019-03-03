
import java.nio.file.{Files, Paths}

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
      val program = Parser.parse(source)
      val symbolTbl = TypeChecker.runSemanticAnalysis(program)

      symbolTbl match {
        case Failure(e) => println(e.getMessage)
                           System.exit(1)
        case Success(tbl) => Interpreter.runProgram(program, tbl)
      }
  }
}
