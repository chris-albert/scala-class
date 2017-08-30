package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    //b² - 4ac
    Signal {
      Math.pow(b(), 2) - (4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a
    Signal {
      val sd = Math.sqrt(delta())
      if(sd >= 0) {
        Set(
          (-b() + sd) / (2 * a()),
          (-b() - sd) / (2 * a())
        )
      } else Set()
    }
  }
}
