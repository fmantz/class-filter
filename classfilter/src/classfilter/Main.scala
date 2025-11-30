package classfilter

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/**
 * Simply calls the essential class filter and writes the result to the path in the first argument
 */
object Main {

  def main(args: Array[String]): Unit = {
    val isWithDependency = args.contains("-D")
    val countMinArgs = if (isWithDependency) 3 else 2
    if (args.length < countMinArgs) {
      throw new IllegalArgumentException("Main [-D] filterPath rootClassName1 [rootClassName2 ...] \nnote: '-D' means with dependencies")
    } else if (isWithDependency) {
      val argsN = args.tail
      val transitiveDependentClassNamesGraph = EssentialClassFilter.getDependencyGraph(argsN.tail *).asScala
      val filterPath = Paths.get(argsN.head)
      Files.write(filterPath,
        transitiveDependentClassNamesGraph
          .map(r => s"${r._1}: ${r._2.mkString(", ")}")
          .mkString("\n").getBytes(StandardCharsets.UTF_8)
      )
    } else {
      val transitiveDependentClassNames = EssentialClassFilter.getClassNames(args.tail *)
      val filterPath = Paths.get(args.head)
      Files.write(filterPath,
        transitiveDependentClassNames.mkString("\n").getBytes(StandardCharsets.UTF_8)
      )
    }
  }

}
