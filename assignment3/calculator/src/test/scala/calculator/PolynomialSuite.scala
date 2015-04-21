package calculator

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, _}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PolynomialSuite extends FunSuite with ShouldMatchers {

  test("test basic discriminant") {
    val a = Var(1.0)
    val b = Var(1.0)
    val c = Var(1.0)

    val delta = Polynomial.computeDelta(a, b, c)

    assert(delta() === -3.0)

    b() = -1.0

    assert(delta() === -3.0)

    b() = 2.0

    assert(delta() === 0)

    c() = -1.0

    assert(delta() === 8)

    a() = -1.0

    assert(delta() === 0)
  }

  test("test no solutions") {
    val a = Var(1.0)
    val b = Var(1.0)
    val c = Var(1.0)

    val delta = Polynomial.computeDelta(a, b, c)

    assert(delta() < 0)

    val sol = Polynomial.computeSolutions(a, b, c, delta)

    assert(0 === sol().size)

    a() = 0.0

    assert(0 === sol().size)
  }

  test("test one solution") {
    val a = Var(1.0)
    val b = Var(2.0)
    val c = Var(1.0)

    val delta = Polynomial.computeDelta(a, b, c)

    assert(delta() === 0)

    val sol = Polynomial.computeSolutions(a, b, c, delta)

    assert(1 === sol().size)
    assert(-1.0 === sol().head)
  }

  test("test two solutions") {
    val a = Var(1.0)
    val b = Var(-2.0)
    val c = Var(0.0)

    val delta = Polynomial.computeDelta(a, b, c)

    assert(delta() === 4.0)

    val sol = Polynomial.computeSolutions(a, b, c, delta)

    assert(2 === sol().size)
    assert(0.0 === sol().head)
    assert(2.0 === sol().tail.head)

    a() = -2.0
    b() = -3.0
    c() = 2.0

    assert(delta() === 25.0)

    assert(2 === sol().size)
    assert(0.5 === sol().head)
    assert(-2.0 === sol().tail.head)
  }

  test("test two solutions - special case") {
    val a = Var(1.0)
    val b = Var(4.0)
    val c = Var(1.0)

    val delta = Polynomial.computeDelta(a, b, c)

    assert(delta() === 12.0)
    println(delta())

    val sol = Polynomial.computeSolutions(a, b, c, delta)

    assert(2 === sol().size)

    println(sol().head)
    println(sol().tail.head)

    assert(-3.732050807568877 === sol().head)
    assert(-0.2679491924311228 === sol().tail.head)

    a() = 0.990420709
    b() = 4.572004784
    c() = 0.866577707

    assert(17.470121717036747 == delta())

    println(sol().head)
    println(sol().tail.head)

    assert(-4.418189247106155 === sol().head)
    assert(-0.19803569897380308 === sol().tail.head)
  }

}
