class Until(expr: => Boolean) {
  def apply(): Boolean = expr
}

object UNTIL {
  def apply(expr: => Boolean) = new Until(expr)
}

object Loops {
  def REPEAT(block: => Unit)(until: Until): Unit = {
    block
    if (!until()) REPEAT(block)(until)
  }
}
