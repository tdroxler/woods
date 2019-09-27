object Foo {

  val foo: String = "foo"

  def main(args: Array[String]): Unit = {
    println(foo)

    val module = Module.module
    println(module)
  }
}
