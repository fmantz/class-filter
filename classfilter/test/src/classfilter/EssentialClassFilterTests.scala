package classfilter

import utest.{TestSuite, Tests, test}
import scala.jdk.CollectionConverters.*

object EssentialClassFilterTests extends TestSuite {
  def tests: Tests = Tests {
    test("find non-standard java types with java classes") {
        val rs = EssentialClassFilter.getClassNames("testjava.A").toSeq
        val exp = Seq(
          "testjava.A",
          "testjava.A$Inner",
          "testjava.A$OtherInner",
          "testjava.B",
          "testjava.C",
          "testjava.D",
          "testjava.E",
          "testjava.F",
          "testjava.G",
          "testjava.H",
          "testjava.I",
          "testjava.J",
          "testjava.K",
          "testjava.MyEnum",
          "testjava.MyException",
          "testjava.S",
          "testjava.SI",
          "testjava.sub.AA",
          "testjava.sub.CAnnotation",
          "testjava.sub.FAnnotation",
          "testjava.sub.MAnnotation",
          "testjava.sub.leaf.AAA",
          "testjava.sub.leaf.OtherException"
        )
        assert(rs == exp)
    }

    test("find non-standard java types with scala classes") {
      val rs = EssentialClassFilter.getClassNames("testscala.A").filter(_.startsWith("testscala")).toSeq
      val exp = Seq(
        "testscala.A",
        "testscala.A$$anon$1",
        "testscala.A$B$",
        "testscala.A$B$C",
        "testscala.A$B$C$",
        "testscala.A$D",
        "testscala.F",
        "testscala.G",
        "testscala.H",
        "testscala.H$",
        "testscala.I$",
        "testscala.J",
        "testscala.K",
        "testscala.L",
        "testscala.MyException"
      )
      assert(rs == exp)
    }

    test("find non-standard java types with a enum given") {
      val rs = EssentialClassFilter.getClassNames(
        "testjava.MyEnum"
      ).toSeq
      val exp = Seq(
        "testjava.MyEnum"
      )
      assert(rs == exp)
    }

    test("find non-standard java types with java classes when there are already most java classes given") {
      val rs = EssentialClassFilter.getClassNames(
        "testjava.A",
        "testjava.B",
        "testjava.C",
        "testjava.D",
        "testjava.E",
        "testjava.F",
        "testjava.G",
        "testjava.H",
        "testjava.I",
        "testjava.J",
        "testjava.K",
        "testjava.MyEnum",
        "testjava.MyException",
        "testjava.S",
        "testjava.SI",
        "testjava.sub.AA",
        "testjava.sub.CAnnotation",
        "testjava.sub.FAnnotation",
        "testjava.sub.MAnnotation",
        "testjava.sub.leaf.AAA",
        "testjava.sub.leaf.OtherException"
      ).toSeq
      val exp = Seq(
        "testjava.A",
        "testjava.A$Inner",
        "testjava.A$OtherInner",
        "testjava.B",
        "testjava.C",
        "testjava.D",
        "testjava.E",
        "testjava.F",
        "testjava.G",
        "testjava.H",
        "testjava.I",
        "testjava.J",
        "testjava.K",
        "testjava.MyEnum",
        "testjava.MyException",
        "testjava.S",
        "testjava.SI",
        "testjava.sub.AA",
        "testjava.sub.CAnnotation",
        "testjava.sub.FAnnotation",
        "testjava.sub.MAnnotation",
        "testjava.sub.leaf.AAA",
        "testjava.sub.leaf.OtherException"
      )
      assert(rs == exp)
    }

    test("find dependencies by class") {
      val rsFormated: Seq[(String, Seq[String])] = EssentialClassFilter
        .getDependencyGraph("testjava.A")
        .asScala
        .toSeq
        .map(d => (d._1, d._2.toSeq))
        .sortBy(_._1)

      val rsExp = List(
        ("testjava.A",            Seq("testjava.A$Inner", "testjava.A$OtherInner", "testjava.B", "testjava.C", "testjava.D", "testjava.E", "testjava.F", "testjava.G", "testjava.H", "testjava.I", "testjava.J", "testjava.K", "testjava.MyEnum", "testjava.MyException", "testjava.S", "testjava.SI", "testjava.sub.AA", "testjava.sub.CAnnotation", "testjava.sub.FAnnotation", "testjava.sub.MAnnotation", "testjava.sub.leaf.AAA", "testjava.sub.leaf.OtherException")),
        ("testjava.A$Inner",      Seq("testjava.A", "testjava.A$OtherInner", "testjava.B", "testjava.C", "testjava.D", "testjava.E", "testjava.F", "testjava.G", "testjava.H", "testjava.I", "testjava.J", "testjava.K", "testjava.MyEnum", "testjava.MyException", "testjava.S", "testjava.SI", "testjava.sub.AA", "testjava.sub.FAnnotation", "testjava.sub.MAnnotation", "testjava.sub.leaf.AAA", "testjava.sub.leaf.OtherException")),
        ("testjava.A$OtherInner", Seq("testjava.A", "testjava.A$Inner", "testjava.B", "testjava.C", "testjava.D", "testjava.E", "testjava.F", "testjava.G", "testjava.H", "testjava.I", "testjava.J", "testjava.K", "testjava.MyEnum", "testjava.MyException", "testjava.S", "testjava.SI", "testjava.sub.AA", "testjava.sub.FAnnotation", "testjava.sub.MAnnotation", "testjava.sub.leaf.AAA", "testjava.sub.leaf.OtherException")),
        ("testjava.B", Seq()),
        ("testjava.C", Seq()),
        ("testjava.D", Seq()),
        ("testjava.E", Seq("testjava.MyException", "testjava.sub.leaf.OtherException")),
        ("testjava.F", Seq()),
        ("testjava.G", Seq()),
        ("testjava.H", Seq()),
        ("testjava.I", Seq()),
        ("testjava.J", Seq()),
        ("testjava.K", Seq()),
        ("testjava.MyEnum", Seq()),
        ("testjava.MyException", Seq()),
        ("testjava.S", Seq()),
        ("testjava.SI", Seq("testjava.S")),
        ("testjava.sub.AA", Seq()),
        ("testjava.sub.CAnnotation", Seq()),
        ("testjava.sub.FAnnotation", Seq()),
        ("testjava.sub.MAnnotation", Seq()),
        ("testjava.sub.leaf.AAA", Seq("testjava.sub.AA")),
        ("testjava.sub.leaf.OtherException", Seq())
      )
      assert(rsFormated == rsExp)
    }

  }
}
