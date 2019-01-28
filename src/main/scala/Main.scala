
import java.nio.file.{Files, Paths}

object Main {
    def main(args: Array[String]) {
      if (args.isEmpty) {
        println("Usage: mini-pl [source file]")
        System.exit(0)
      }
      if (!Files.exists(Paths.get(args(0)))) {
        println("Given file does not exist")
        System.exit(0)
      }
      // TODO
  }
}
