import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class RepeatUntilSuite extends FunSuite with ShouldMatchers {

  test("test") {
    var counter = 0

    Loops.REPEAT({ counter += 1}) UNTIL( {counter == 10} )
  }
}
