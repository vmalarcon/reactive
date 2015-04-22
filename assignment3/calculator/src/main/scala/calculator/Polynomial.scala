package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(Math.pow(b(), 2.0) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(if (delta() < 0 || a() == 0) Set()
    else {
      val root1 = Var((-b() - Math.sqrt(delta())) / (2 * a()))
      val root2 = Var((-b() + Math.sqrt(delta())) / (2 * a()))

      if (root1 == root2) Set(root1()) else Set(root1(), root2())
    })
  }
}
