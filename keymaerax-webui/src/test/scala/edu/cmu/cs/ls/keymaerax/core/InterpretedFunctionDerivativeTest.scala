package edu.cmu.cs.ls.keymaerax.core

import edu.cmu.cs.ls.keymaerax.Configuration
import edu.cmu.cs.ls.keymaerax.btactics.InvariantGenerator.BelleExprProofHint
import edu.cmu.cs.ls.keymaerax.btactics.TacticTestBase
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.tools.qe.{KeYmaeraToMathematica, MathematicaToKeYmaera}

class InterpretedFunctionDerivativeTest extends TacticTestBase {
  import edu.cmu.cs.ls.keymaerax.btactics.TactixLibrary._

  it should "prove forall x (exp(x) > 0)" in withMathematica { qe =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)

    val exp = "exp(x) > 0".asFormula

    val pr = proveBy(exp, QE)

    pr shouldBe 'proved
  }

  it should "prove d/dx exp(x) = exp(x)" in withMathematica { qe =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)

    val exp = "(exp(2*x))' = 2*exp(x)".asFormula
  }
}
