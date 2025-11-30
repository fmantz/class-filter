package testscala

class A {

  object B {
    enum C {
      case X, Y, Z
    }

    def b(): Unit = {
      I.i()
    }

  }

  class D {
    val f:F = null

    def g()(using l:L): G = {
      val h: H = null
      val x = (j:J) => new K()
      null
    }

    def e(): Unit = {
      throw new MyException
    }

  }

}
