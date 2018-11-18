import week1.Generators._

object rgenerator  {

  integers.generate
  integers.generate
  integers.generate
  integers.generate

  lists.generate

  trees.generate

  trees.generate
  trees.generate


  trees.generate


  oneOf("red", "blue", "yellow").generate
  oneOf(1,2,3,4,5).generate


  /** Random Test Function */
  //an important application of random number generator is application testing
  //ScalaCheck is a tool you can use for random testing
  def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
    for(i<- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "test failed for "+value)
    }
    println("passed "+numTimes+" tests")
  }

  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }

}
