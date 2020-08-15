package edu.cmu.cs.ls.keymaerax.btactics

import edu.cmu.cs.ls.keymaerax.Configuration
import edu.cmu.cs.ls.keymaerax.btactics.TactixLibrary._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._

class TrigTests extends TacticTestBase {
  "trig interpreted fns" should "solve sin statements with QE" in withMathematica { _ =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    val formula = "[ x:= x0 ; ] sin(x + y) = -sin(-x0 - y) + sin(0)".asFormula
    val proof = proveBy(formula,
      chase(1) & QE)
    proof shouldBe 'proved
  }

  it should "solve cos statements with QE" in withMathematica { _ =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    val formula = "[ x:= y ; y:=z ; ] cos(0) >= cos(x*y)".asFormula
    val proof = proveBy(formula,
      chase(1) & QE)
    proof shouldBe 'proved
  }

  it should "prove trig identities with QE" in withMathematica { _ =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    Configuration.set(Configuration.Keys.LOG_QE, "true", saveToFile = true)
    val formula = "[x := x0 ; ](cos(x))*(cos(x)) + (sin(x))*(sin(x)) = 1".asFormula
    val proof = proveBy(formula, chase(1) & QE(timeout = Some(30)))
    proof shouldBe 'proved
  }
}
