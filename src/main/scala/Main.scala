
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

    val res = for {
      st <- TypeChecker.runSemanticAnalysis(program)
      result <- Interpreter.runProgram(program, st)
    } yield result

    res match {
      case Success(_) => System.exit(0)
      case Failure(e) => println(e + " \n" + e.getMessage)
    }
  }

}
