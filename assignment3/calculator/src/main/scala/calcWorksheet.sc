import calculator._

val one = Literal(1)
val plus = Plus(Ref("a"), one)

val variables: Map[String, Signal[Expr]] = Map("a" -> Var(Literal(1.0)))

Calculator.eval(plus, variables)


