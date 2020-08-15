package edu.cmu.cs.ls.keymaerax.btactics

import edu.cmu.cs.ls.keymaerax.Configuration
import edu.cmu.cs.ls.keymaerax.btactics.TactixLibrary._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._

class ExpTests extends TacticTestBase {
  "exp interpreted fn" should "apply exp identities with QE" in withMathematica { _ =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    val formula = "[ x:= y ; y:=z ; ] exp(x+y^2) > 0".asFormula
    val proof = proveBy(formula,
      chase(1) & QE)
    proof shouldBe 'proved
  }

  it should "prove algebra on exp with QE" in withMathematica { _ =>
    // This is the formula that the last test (with dgDbx) fails to solve
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    val formula = ("[y':=(-k())*y+0;][t':=1;][x':=k()*x;]" +
                   "y'*(x-exp(k()*t))+y*(x'-(k()*t)'*exp(k()*t))=0").asFormula
    val proof = proveBy(formula,
      derive(1, 1::1::1::0::1::1::1::0::Nil) & chase(1) & QE
    )
    proof shouldBe 'proved
  }

  it should "simplify exp differentiation with DS" in withMathematica { _ =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    val formula = "(exp(x) > 5) -> [{x' = 1}] (exp(x) > 5)".asFormula
    val proof = proveBy(formula,
      implyR(1) &
        dI()(1)
//        dI('diffInd)(1) <(
//          closeId,
//          derive(1, 1::0::0::Nil) & chase(1) & QE
//        )
      )
    proof shouldBe 'proved
  }

  it should "prove exp solutions via dbx" in withMathematica { qeTool =>
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    val formula = "x=1 & t=0 -> [{x'=k*x,t'=1}] x - exp(k*t) = 0".asFormula
    val proof = proveBy(formula,
      implyR(1) & andL(-1) & DifferentialTactics.dgDbx("k".asTerm)(1)
    )
    println(proof)
    proof shouldBe 'proved
  }
}
