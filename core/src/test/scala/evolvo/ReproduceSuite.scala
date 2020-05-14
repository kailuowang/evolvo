package evolvo

class ReproduceSuite extends munit.FunSuite {

  test("Mate") {
    val children =
      Reproduction(-5, 10, 2.3, 0.5).mate(Individual(100), Individual(100))
    println(children)
    assert(clue(children.size) > 0)
  }

}
