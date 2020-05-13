package evolvo

class ReproduceSuite extends munit.FunSuite {

  test("Mate") {
    val child = Reproduction(-5, 10).mate(Individual(100), Individual(100))
    assert(clue(child.power) > 0, "Always larger than zero")
  }

}
