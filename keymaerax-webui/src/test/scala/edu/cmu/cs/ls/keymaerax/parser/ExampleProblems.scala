/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/
package edu.cmu.cs.ls.keymaerax.parser

import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

import org.scalatest.OptionValues._

/**
 * @author Nathan Fulton
 */
class ExampleProblems extends FlatSpec with Matchers with BeforeAndAfterEach {
  override def beforeEach(): Unit = { PrettyPrinter.setPrinter(KeYmaeraXPrettyPrinter.pp) }

  "parser line messages" should "be properly offset" in {
    val f =
      """|   ProgramVariables.
        |   R x. R y. R z.
        |   End.
        |   Problem.
        |   (x*x*y >= 0 & x >= 0 & z >= x)
        |   ->
        |   [
        |   {x := 2x; y := 2y;}
        |  ]
        | (x*y >= 0)
        |End.""".stripMargin

    val ex = the [ParseException] thrownBy KeYmaeraXArchiveParser(f)
    ex.found shouldBe """x"""
    ex.expect shouldBe "Multiplication in KeYmaera X requires an explicit * symbol. E.g. 2*term"
    ex.loc.begin.line shouldBe 8
    ex.loc.begin.column shouldBe 11
  }

  it should "be properly offset in another example" in {
    val f =
      """ ProgramVariables.
        |
        |   R x. R y. R z.
        |
        |   End.
        |
        |   Problem.
        |
        |   (x*x*y >= 0 & x >= 0 & z >= x)
        |
        |   ->
        |
        |   [
        |
        |   {x := 2x; y := 2y;}
        |
        |  ]
        |
        | (x*y >= 0)
        |
        |End.""".stripMargin

    val ex = the [ParseException] thrownBy KeYmaeraXArchiveParser.parseAsProblemOrFormula(f)
    ex.found shouldBe """x"""
    ex.expect shouldBe "Multiplication in KeYmaera X requires an explicit * symbol. E.g. 2*term"
    ex.loc.begin.line shouldBe 15
    ex.loc.begin.column shouldBe 11
  }

  it should "work from file input" in {
    val f = " ProgramVariables.\n\n   R x. R y. R z. \n\n   End.  \n\n   Problem.\n\n   (x*x*y >= 0 & x >= 0 & z >= x) \n\n   -> \n\n   [\n\n   {x := 2x; y := 2y;}\n\n  ]\n\n (x*y >= 0)\n\nEnd."

    val ex = the [ParseException] thrownBy KeYmaeraXArchiveParser.parseAsProblemOrFormula(f)
    ex.found shouldBe """x"""
    ex.expect shouldBe "Multiplication in KeYmaera X requires an explicit * symbol. E.g. 2*term"
    ex.loc.begin.line shouldBe 15
    ex.loc.begin.column shouldBe 11
  }

  "The Parser" should "parse a simple file" in {
    val theProblem =
      """
        |Functions.
        |  R f(R, R).
        |  R g(R).
        |End.
        |ProgramVariables.
        |  R x.
        |  R y.
        |End.
        |Problem.
        |  x > 0 ->
        |  [
        |   x := x + 1;
        |  ] x > 0
        |End.
      """.stripMargin

    val result = KeYmaeraXArchiveParser(theProblem)
    result should have size 1
    result.head.model shouldBe "x > 0 -> [x := x + 1;] x > 0".asFormula
  }

  it should "parse x>0 -> \\exists d [{x'=d}]x>0" in {
    val problem = "x>0 -> \\exists d [{x'=d}]x>0"
    val declProblem =
      s"""
        |ProgramVariables.
        |  R x.
        |End.
        |
        |Problem.
        |  $problem
        |End.
      """.stripMargin

    KeYmaeraXArchiveParser(declProblem).head.model shouldBe KeYmaeraXParser(problem)
  }

  "Interpretation parser" should "parse simple function declarations" in {
    val problem =
      """
        |Functions.
        |  R f() = (5).
        |End.
        |
        |Problem.
        |  f()>3
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should have size 1
    entry.defs.decls should contain key ("f", None)
    entry.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Unit
      sort shouldBe Real
      argNames shouldBe Some(Nil)
      interpretation.value shouldBe Number(5)
    }
    entry.model shouldBe KeYmaeraXParser("f()>3")
  }

  it should "parse function declarations with one parameter" in {
    val problem =
      """
        |Functions.
        |  R f(R) = (5 + .*.).
        |End.
        |
        |Problem.
        |  f(4)>3
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should have size 1
    entry.defs.decls should contain key ("f", None)
    entry.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Real
      sort shouldBe Real
      argNames shouldBe Some((("\\cdot", Some(0)), Real) :: Nil)
      interpretation.value shouldBe Plus(Number(5), Times(DotTerm(Real), DotTerm(Real)))
    }
    entry.model shouldBe KeYmaeraXParser("f(4)>3")
  }

  it should "parse n-ary function declarations" in {
    val problem =
      """
        |Functions.
        |  R f(R,R,R) = (._0 + ._1*._2).
        |End.
        |
        |Problem.
        |  f(2,3,4)>3
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should have size 1
    entry.defs.decls should contain key ("f", None)
    entry.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Tuple(Real, Tuple(Real, Real))
      sort shouldBe Real
      argNames shouldBe Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: (("\\cdot", Some(2)), Real) :: Nil)
      interpretation.value shouldBe Plus(DotTerm(Real, Some(0)), Times(DotTerm(Real, Some(1)), DotTerm(Real, Some(2))))
    }
    entry.model shouldBe KeYmaeraXParser("f(2,3,4)>3")
  }

  it should "detect undeclared dots" in {
    val problem =
      """
        |Functions.
        |  R f(R,R,R) = (._1 + ._2*._3).
        |End.
        |
        |Problem.
        |  f(2,3,4)>3
        |End.
      """.stripMargin

    val thrown = the [ParseException] thrownBy KeYmaeraXArchiveParser.parseProblem(problem)
    thrown.getMessage should include ("Function/predicate f(•_0,•_1,•_2) defined using undeclared •_3")
  }

  it should "replace names with the appropriate dots" in {
    val problem =
      """Functions.
        |  R f(R x, R y, R z) = (x + y*z).
        |End.
        |
        |Problem.
        |  f(2,3,4)>3
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should have size 1
    entry.defs.decls should contain key ("f", None)
    entry.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Tuple(Real, Tuple(Real, Real))
      sort shouldBe Real
      argNames shouldBe Some((("x", None), Real) :: (("y", None), Real) :: (("z", None), Real) :: Nil)
      interpretation.value shouldBe Plus(DotTerm(Real, Some(0)), Times(DotTerm(Real, Some(1)), DotTerm(Real, Some(2))))
    }
    entry.model shouldBe KeYmaeraXParser("f(2,3,4)>3")
  }

  it should "not confuse arguments of same name across definitions" in {
    val problem =
      """Definitions.
        |  R f(R x, R y, R z) = (x+y*z).
        |  R g(R a, R x, R y) = (f(x,y,a)).
        |End.
        |
        |Problem.
        |  f(1,2,3)>0 -> g(3,1,2)>0
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should have size 2
    entry.defs.decls should contain key ("f", None)
    entry.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Tuple(Real, Tuple(Real, Real))
      sort shouldBe Real
      argNames shouldBe Some((("x", None), Real) :: (("y", None), Real) :: (("z", None), Real) :: Nil)
      interpretation.value shouldBe Plus(DotTerm(Real, Some(0)), Times(DotTerm(Real, Some(1)), DotTerm(Real, Some(2))))
    }
    entry.defs.decls should contain key ("g", None)
    entry.defs.decls(("g", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Tuple(Real, Tuple(Real,Real))
      sort shouldBe Real
      argNames shouldBe Some((("a", None), Real) :: (("x", None), Real) :: (("y", None), Real) :: Nil)
      interpretation.value shouldBe FuncOf(Function("f", None, Tuple(Real, Tuple(Real, Real)), Real),
        Pair(DotTerm(Real, Some(1)), Pair(DotTerm(Real, Some(2)), DotTerm(Real, Some(0)))))
    }
    entry.model shouldBe KeYmaeraXParser("f(1,2,3)>0 -> g(3,1,2)>0")
  }

  it should "correctly dottify in the presence of unused arguments" in {
    val problem =
      """Definitions.
        |  R f(R x, R y) = (y+3).
        |End.
        |
        |Problem.
        |  f(1,2)>0
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should have size 1
    entry.defs.decls should contain key ("f", None)
    entry.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Tuple(Real, Real)
      sort shouldBe Real
      argNames shouldBe Some((("x", None), Real) :: (("y", None), Real) :: Nil)
      interpretation.value shouldBe Plus(DotTerm(Real, Some(1)), Number(3))
    }
    entry.model shouldBe KeYmaeraXParser("f(1,2)>0")
  }

  it should "replace argument name of unary function with non-indexed dot (for backwards compatibility)" in {
    val problem =
      """Functions.
        |  R f(R x) = (x + 2).
        |End.
        |
        |Problem.
        |  f(2)>3
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should have size 1
    entry.defs.decls should contain key ("f", None)
    entry.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Real
      sort shouldBe Real
      argNames shouldBe Some((("x", None), Real) :: Nil)
      interpretation.value shouldBe Plus(DotTerm(Real), Number(2))
    }
    entry.model shouldBe KeYmaeraXParser("f(2)>3")
  }

  it should "allow both . and explicit ._0 in unary function definition" ignore {
    def problem(dot: String): String =
      s"""Functions.
        |  R f(R) = ($dot + 2).
        |End.
        |
        |Problem.
        |  f(2)>3
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem("._0"))
    entry.defs.decls should have size 1
    entry.defs.decls should contain key ("f", None)
    entry.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Real
      sort shouldBe Real
      argNames shouldBe 'empty
      interpretation.value shouldBe Plus(DotTerm(Real), Number(2))
    }
    entry.model shouldBe KeYmaeraXParser("f(2)>3")

    val entry2 = KeYmaeraXArchiveParser.parseProblem(problem("."))
    entry2.defs.decls should have size 1
    entry2.defs.decls should contain key ("f", None)
    entry2.defs.decls(("f", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Real
      sort shouldBe Real
      argNames shouldBe 'empty
      interpretation.value shouldBe Plus(DotTerm(Real), Number(2))
    }
    entry2.model shouldBe KeYmaeraXParser("f(2)>3")
  }

  it should "parse program definitions" in {
    val problem =
      """
        |Functions.
        |  HP a ::= { x:=*; ?x<=5; ++ x:=y; }.
        |End.
        |
        |ProgramVariables.
        |  R x.
        |  R y.
        |End.
        |
        |Problem.
        |  y<=5 -> [a;]x<=5
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should contain key ("a", None)
    entry.defs.decls(("a", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Unit
      sort shouldBe Trafo
      argNames shouldBe 'empty
      interpretation.value shouldBe "x:=*; ?x<=5; ++ x:=y;".asProgram
    }
    entry.model shouldBe "y<=5 -> [a;]x<=5".asFormula
  }

  it should "parse functions and predicates in program definitions" in {
    val problem =
      """
        |Functions.
        |  R f(R) = (.+2).
        |  B p(R,R) <-> (._0<=._1).
        |  HP a ::= { x:=*; ?p(x,5); ++ x:=f(y); }.
        |End.
        |
        |ProgramVariables.
        |  R x.
        |  R y.
        |End.
        |
        |Problem.
        |  y<=5 -> [a;]x<=5
        |End.
      """.stripMargin

    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should contain key ("a", None)
    entry.defs.decls(("a", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Unit
      sort shouldBe Trafo
      argNames shouldBe 'empty
      interpretation.value shouldBe "x:=*; ?p(x,5); ++ x:=f(y);".asProgram
    }
    entry.model shouldBe "y<=5 -> [a;]x<=5".asFormula
  }

  it should "parse annotations" in {
    val problem =
      """
        |Functions.
        |  B p(R,R) <-> (._0>=._1).
        |  HP loopBody ::= { x:=x+2; }.
        |  HP a ::= { x:=1; {loopBody;}*@invariant(p(x,1)) }.
        |End.
        |
        |ProgramVariables.
        |  R x.
        |End.
        |
        |Problem.
        |  [a;]x>=1
        |End.
      """.stripMargin

    var annotation: Option[(Program, Formula)] = None
    KeYmaeraXParser.setAnnotationListener((prg, fml) => annotation = Some(prg -> fml))
    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should contain key ("a", None)
    entry.defs.decls(("a", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Unit
      sort shouldBe Trafo
      argNames shouldBe 'empty
      interpretation.value shouldBe "x:=1; {loopBody;}*".asProgram
    }
    entry.defs.decls(("loopBody", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Unit
      sort shouldBe Trafo
      argNames shouldBe 'empty
      interpretation.value shouldBe "x:=x+2;".asProgram
    }
    annotation shouldBe Some("{loopBody;}*".asProgram, "p(x,1)".asFormula)
    entry.model shouldBe "[a;]x>=1".asFormula
  }

  it should "be allowed to ignore later definitions when elaborating annotations" in {
    val problem =
      """
        |Functions.
        |  HP a ::= { x:=1; {loopBody;}*@invariant(p(x,1)) }.
        |  B p(R,R) <-> (._0>=._1).
        |  HP loopBody ::= { x:=x+2; }.
        |End.
        |
        |ProgramVariables.
        |  R x.
        |End.
        |
        |Problem.
        |  [a;]x>=1
        |End.
      """.stripMargin

    var annotation: Option[(Program, Formula)] = None
    KeYmaeraXParser.setAnnotationListener((prg, fml) => annotation = Some(prg -> fml))
    val entry = KeYmaeraXArchiveParser.parseProblem(problem)
    entry.defs.decls should contain key ("a", None)
    entry.defs.decls(("a", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Unit
      sort shouldBe Trafo
      argNames shouldBe 'empty
      interpretation.value shouldBe "x:=1; {loopBody;}*".asProgram
    }
    entry.defs.decls(("loopBody", None)) match { case (domain, sort, argNames, interpretation, _) =>
      domain.value shouldBe Unit
      sort shouldBe Trafo
      argNames shouldBe 'empty
      interpretation.value shouldBe "x:=x+2;".asProgram
    }
    annotation shouldBe 'defined
    annotation.get._1 should (be ("{x:=x+2;}*".asProgram) or be ("{loopBody;}*".asProgram))
    annotation.get._2 should (be ("x>=1".asFormula) or be ("p(x,1)".asFormula))
    entry.model shouldBe "[a;]x>=1".asFormula
  }
}
