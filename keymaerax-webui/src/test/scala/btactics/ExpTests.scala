package btactics

import edu.cmu.cs.ls.keymaerax.Configuration
import edu.cmu.cs.ls.keymaerax.btactics.{Ax, AxiomaticODESolver, TacticTestBase}
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.btactics.TactixLibrary._

class ExpTests extends TacticTestBase {
  it should "solve exp statements with QE" in withMathematica { _ =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    val formula = "[ x:= y ; y:=z ; ] exp(x+y^2) > 0".asFormula
    val proof = proveBy(formula,
      chase(1) & QE)
    proof shouldBe 'proved
  }

  it should "simplify exp differentiation with DS" in withMathematica { _ =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    val formula = "(exp(x) > 5) -> [{x' = 1}] (exp(x) > 5)".asFormula
    val proof = proveBy(formula,
      implyR(1) &
        DI(1) &
        implyR(1) & andR(1) <(
          closeId,
          derive(1, 1::Nil) & useAt(Ax.Dvar)(1, 1::0::0::Nil) & DE(1) & G(1) & Dassignb(1) & QE
        )
      )
    proof shouldBe 'proved
  }
}
