package simulations

import org.scalatest.FunSuiteLike

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuiteLike {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or 4")

    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 5")
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or 4")

    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 5")
  }

  test("demux test") {
    val in = new Wire
    val lstC = List.fill(10) { new Wire }
    val lstOut = List.fill(10) { new Wire }
    demux(in, lstC, lstOut)

    lstC.foreach(_.setSignal(true))
    run

    assert(lstOut.forall(_.getSignal == false))

    in.setSignal(true)
    run

    assert(lstOut.forall(_.getSignal == true))

    lstC.splitAt(5)._1.foreach(_.setSignal(false))
    run

    assert((lstOut.size - 5) == lstOut.count(_.getSignal == true))
  }
}
