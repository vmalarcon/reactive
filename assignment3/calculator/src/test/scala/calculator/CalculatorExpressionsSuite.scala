package calculator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, _}

@RunWith(classOf[JUnitRunner])
class CalculatorExpressionsSuite extends FunSuite with ShouldMatchers {

  test("test basic expressions") {
    val one = Literal(1)
    val plus = Plus(Ref("a"), one)

    val variables: Map[String, Signal[Expr]] = Map("a" -> Var(Literal(1.0)))
    val result = Calculator.eval(plus, variables)

    assert(result == 2)
  }

  test("test variable not found") {
    val result = Calculator.eval(Ref("x"), Map())
    assert(result.isNaN)
  }

  test("test cyclic dep") {
    val one = Literal(1)
    val two = Literal(2)
    val a = Plus(Ref("b"), one)
    val b = Times(Ref("a"), two)

    val variables: Map[String, Signal[Expr]] = Map("a" -> Var(a), "b" -> Var(b))
    val resultA = Calculator.eval(Ref("a"), variables)
    val resultB = Calculator.eval(Ref("b"), variables)

    assert(resultA.isNaN)
    assert(resultB.isNaN)
  }
}
