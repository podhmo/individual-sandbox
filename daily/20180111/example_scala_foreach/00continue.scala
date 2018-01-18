object Main {
  def main(args: Array[String]): Unit = {
    val L = List(1, 2, 3)

    L.foreach { x =>
      println(x)
    }

    println("----------------------------------------")

    def p(x:Int) {
      if (x == 2) {
        return
      }
      println(x)
    }
    L.foreach(p)

    println("----------------------------------------")
    L.foreach { x =>
      println(x)
      if (x == 2) {
        return
      }
    }

    // not reached
    println("----------------------------------------")
  }
}
