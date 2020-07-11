/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/

package edu.cmu.cs.ls.keymaerax.parser

import edu.cmu.cs.ls.keymaerax.bellerophon.{Expand, ExpandAll, PartialTactic}
import edu.cmu.cs.ls.keymaerax.btactics.{DebuggingTactics, TacticTestBase, TactixLibrary}
import edu.cmu.cs.ls.keymaerax.btactics.TactixLibrary._
import edu.cmu.cs.ls.keymaerax.core.{Assign, Bool, Function, Number, Plus, Real, SubstitutionPair, Trafo, Tuple, USubst, USubstOne, Unit, Variable}
import edu.cmu.cs.ls.keymaerax.infrastruct.RenUSubst
import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXArchiveParser.{Declaration, ParsedArchiveEntry}
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import org.scalatest.LoneElement._
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.{MatchResult, Matcher}

/**
  * Tests the archive parser.
  * Created by smitsch on 12/29/16.
  * @author Stefan Mitsch
  * @author Andre Platzer
  */
class KeYmaeraXArchiveParserTests extends TacticTestBase with PrivateMethodTester {
  private val parser =
//    KeYmaeraXArchiveParser
  DLArchiveParser

  private def parse(input: String): List[KeYmaeraXArchiveParser.ParsedArchiveEntry] =
    parser.parse(input)

  private def beDecl(right: Declaration) =
    new Matcher[Declaration] {
      def apply(left: Declaration): MatchResult =
        MatchResult(
          //compare without locations
          left.decls.map(v => v._1 -> (v._2._1, v._2._2, v._2._3)) == right.decls.map(v => v._1 -> (v._2._1, v._2._2, v._2._3)),
          left + " was not " + right,
          left + " was " + right
        )
    }

  "Archive parser" should "parse a model only entry" in {
    val input =
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.problemContent shouldBe input
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input
  }

  it should "parse a model with entry ID" in {
    val input =
      """ArchiveEntry b01_8entry1_and_more_underscores : "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe Map("id" -> "b01_8entry1_and_more_underscores")
    entry.fileContent shouldBe input
  }

  it should "parse a model with entry ID repeated at end" in {
    val input =
      """ArchiveEntry b01_entry1 : "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End b01_entry1.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe Map("id" -> "b01_entry1")
    entry.fileContent shouldBe input
  }

  it should "parse simple function declaration" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Real f(); End.
        | ProgramVariables Real x; End.
        | Problem x>=0 -> f()>0 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
      )))
    entry.model shouldBe "x>=0 -> f()>0".asFormula
    entry.expandedModel shouldBe "x>=0 -> f()>0".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple function definition" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Real f() = (1); End.
        | ProgramVariables Real x; End.
        | Problem x>=0 -> f()>0 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Unit), Real, Some(Nil), Some("1".asTerm), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
      )))
    entry.model shouldBe "x>=0 -> f()>0".asFormula
    entry.expandedModel shouldBe "x>=0 -> 1>0".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple predicate declaration" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Bool p(Real x); End.
        | ProgramVariables Real x; End.
        | Problem p(x) & x>0 -> [x:=x+1;]p(x) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("p", None) -> (Some(Real), Bool, Some((("x", None), Real) :: Nil), None, UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "p(x) & x>0 -> [x:=x+1;]p(x)".asFormula
    entry.expandedModel shouldBe "p(x) & x>0 -> [x:=x+1;]p(x)".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple nullary predicate definition" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Bool p() <-> (2>1); End.
        | ProgramVariables Real x; End.
        | Problem p() & x>0 -> [x:=x+1;]p() End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("p", None) -> (Some(Unit), Bool, Some(Nil), Some("2>1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "p() & x>0 -> [x:=x+1;]p()".asFormula
    entry.expandedModel shouldBe "2>1 & x>0 -> [x:=x+1;]2>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple nullary predicate definition with multiple variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Bool p() <-> (2>1); End.
        | ProgramVariables Real x; Real y; End.
        | Problem p() & x>0 -> [x:=x+y;]p() End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("p", None) -> (Some(Unit), Bool, Some(Nil), Some("2>1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "p() & x>0 -> [x:=x+y;]p()".asFormula
    entry.expandedModel shouldBe "2>1 & x>0 -> [x:=x+y;]2>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple unary predicate definition" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Bool p(Real x) <-> (x>1); End.
        | ProgramVariables Real x; End.
        | Problem p(x) & x>0 -> [x:=x+1;]p(x) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("p", None) -> (Some(Real), Bool, Some((("x", None), Real) :: Nil), Some(".>1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "p(x) & x>0 -> [x:=x+1;]p(x)".asFormula
    entry.expandedModel shouldBe "x>1 & x>0 -> [x:=x+1;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple program declaration before variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions HP a; End.
        | ProgramVariables Real x; End.
        | Problem x!=0 -> [a;]x>1 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("a", None) -> (Some(Unit), Trafo, None, None, UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x!=0 -> [a;]x>1".asFormula
    entry.expandedModel shouldBe "x!=0 -> [a;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse an isolated simple definition assignment" in {
    DLParser.programParser("x:=x+1;") shouldBe Assign(Variable("x"),Plus(Variable("x"),Number(BigDecimal(1))))
    DLParser.programParser("{ x:=x+1; }") shouldBe (DLParser.programParser("x:=x+1;"))
    DLParser.parseValue( "HP a ::= { x:=x+1; };", DLArchiveParser.progDef(_)) shouldBe (("a", None), (None, Trafo, None, Some("x:=x+1;".asProgram), UnknownLocation))
    DLParser.parseValue( "Definitions HP a ::= { x:=x+1; }; End.", DLArchiveParser.definitions(_)) shouldBe (Declaration(Map(("a", None) -> (None, Trafo, None, Some("x:=x+1;".asProgram), UnknownLocation))))
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions HP a ::= { x:=x+1; }; End.
        | ProgramVariables Real x; End.
        | Problem x!=0 -> [a;]x>1 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("a", None) -> (Some(Unit), Trafo, None, Some("x:=x+1;".asProgram), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x!=0 -> [a;]x>1".asFormula
    entry.expandedModel shouldBe "x!=0 -> [x:=x+1;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple program definition assignment before variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions HP a ::= { x:=x+1; }; End.
        | ProgramVariables Real x; End.
        | Problem x!=0 -> [a;]x>1 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("a", None) -> (Some(Unit), Trafo, None, Some("x:=x+1;".asProgram), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x!=0 -> [a{|^@|};]x>1".asFormula
    entry.expandedModel shouldBe "x!=0 -> [x:=x+1;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple program definition test before variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions HP a ::= { ?x>1; }; End.
        | ProgramVariables Real x; End.
        | Problem x!=0 -> [a;]x>1 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("a", None) -> (Some(Unit), Trafo, None, Some("?x>1;".asProgram), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x!=0 -> [a{|^@|};]x>1".asFormula
    entry.expandedModel shouldBe "x!=0 -> [?x>1;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple program definition ODE before variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions HP a ::= { {x'=5} }; End.
        | ProgramVariables Real x; End.
        | Problem x!=0 -> [a;]x>1 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("a", None) -> (Some(Unit), Trafo, None, Some("{x'=5}".asProgram), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x!=0 -> [a{|^@|};]x>1".asFormula
    entry.expandedModel shouldBe "x!=0 -> [{x'=5}]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple program definition compose assign before variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions HP a ::= { x:=x+1;?x>1; }; End.
        | ProgramVariables Real x; End.
        | Problem x!=0 -> [a;]x>1 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("a", None) -> (Some(Unit), Trafo, None, Some("x:=x+1;?x>1;".asProgram), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x!=0 -> [a{|^@|};]x>1".asFormula
    entry.expandedModel shouldBe "x!=0 -> [x:=x+1;?x>1;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse simple program definition compose before variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions HP a ::= { ?x>1;x:=x+1; }; End.
        | ProgramVariables Real x; End.
        | Problem x!=0 -> [a;]x>1 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("a", None) -> (Some(Unit), Trafo, None, Some("?x>1;x:=x+1;".asProgram), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x!=0 -> [a{|^@|};]x>1".asFormula
    entry.expandedModel shouldBe "x!=0 -> [?x>1;x:=x+1;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse lots of definitions before variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Real f() = (1); Bool p(Real x) <-> (x>1); Bool q(Real x, Real y, Real z) <-> (x+y>z); HP a ::= { ?p(x); }; End.
        | ProgramVariables Real x; Real y; End.
        | Problem p(x) & y>=0 -> q(x,y,f()) & [a;]p(x) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Unit), Real, Some(Nil), Some("1".asTerm), UnknownLocation),
        ("p", None) -> (Some(Real), Bool, Some((("x", None), Real) :: Nil), Some(".>1".asFormula), UnknownLocation),
        ("q", None) -> (Some(Tuple(Real, Tuple(Real, Real))), Bool, Some((("x", None), Real) :: (("y", None), Real) :: (("z", None), Real) :: Nil), Some("._0+._1>._2".asFormula), UnknownLocation),
        ("a", None) -> (Some(Unit), Trafo, None, Some("?p(x);".asProgram), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "p(x) & y>=0 -> q(x,y,f()) & [a{|^@|};]p(x)".asFormula
    entry.expandedModel shouldBe "x>1 & y>=0 -> x+y>1 & [?x>1;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "complain when variable and function have same name" in {
    val input =
      """ArchiveEntry "Entry 1"
        | Definitions Real x() = 2; End.
        | ProgramVariables Real x; End.
        | Problem x=1 -> x<=x() End.
        |End.""".stripMargin
    the [ParseException] thrownBy parse(input) should have message
      """<somewhere> Semantic analysis error
        |semantics: Expect unique names_index that identify a unique type.
        |ambiguous: x:Unit->Real and x:Real
        |symbols:   x, x
        |Found:    <unknown> at <somewhere>
        |Expected: <unknown>""".stripMargin
  }

  it should "parse simple definitions after variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; End.
        | Definitions Real f() = (1); End.
        | Problem f()>0 & x>=0 -> [x:=x+1;]f()>0 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Unit), Real, Some(Nil), Some("1".asTerm), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
      )))
    entry.model shouldBe "f()>0 & x>=0 -> [x:=x+1;]f()>0".asFormula
    entry.expandedModel shouldBe "1>0 & x>=0 -> [x:=x+1;]1>0".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse lots of definitions after variables" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Definitions Real f() = (1); Bool p(Real x) <-> (x>1); Bool q(Real x, Real y, Real z) <-> (x+y>z); HP a ::= { ?p(x); }; End.
        | Problem p(x) & y>=0 -> q(x,y,f()) & [a;]p(x) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Unit), Real, Some(Nil), Some("1".asTerm), UnknownLocation),
        ("p", None) -> (Some(Real), Bool, Some((("x", None), Real) :: Nil), Some(".>1".asFormula), UnknownLocation),
        ("q", None) -> (Some(Tuple(Real, Tuple(Real, Real))), Bool, Some((("x", None), Real) :: (("y", None), Real) :: (("z", None), Real) :: Nil), Some("._0+._1>._2".asFormula), UnknownLocation),
        ("a", None) -> (Some(Unit), Trafo, None, Some("?p(x);".asProgram), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "p(x) & y>=0 -> q(x,y,f()) & [a{|^@|};]p(x)".asFormula
    entry.expandedModel shouldBe "x>1 & y>=0 -> x+y>1 & [?x>1;]x>1".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "accept ODEs without extra braces" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real t; End.
        | Definitions HP a ::= { x'=x, t'=1 & x<=2 }; End.
        | Problem [a;]x<=2 End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("a", None) -> (Some(Unit), Trafo, None, Some("{ x'=x, t'=1 & x<=2 }".asProgram), UnknownLocation),
        ("t", None) -> (None, Real, None, None, UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "[a{|^@|};]x<=2".asFormula
    entry.expandedModel shouldBe "[{x'=x, t'=1 & x<=2}]x<=2".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse definitions with dot arguments" in {
    val input =
      """ArchiveEntry "Entry 1"
        | Definitions Real f(Real); Real g(Real,Real); Real h(Real) = (.+2); End.
        | ProgramVariables Real x; Real y; End.
        | Problem f(x)>g(x,y) & h(x)>5 End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Real), Real, Some((("\\cdot", Some(0)), Real) :: Nil), None, UnknownLocation),
        ("g", None) -> (Some(Tuple(Real, Real)), Real, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), None, UnknownLocation),
        ("h", None) -> (Some(Real), Real, Some((("\\cdot", Some(0)), Real) :: Nil), Some(".+2".asTerm), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "f(x)>g(x,y) & h(x)>5".asFormula
    entry.expandedModel shouldBe "f(x)>g(x,y) & x+2>5".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse definitions without parentheses" in {
    val input = """ArchiveEntry "Entry 1"
                  | Definitions Real f() = 5; Bool p(Real x) <-> x>0; End.
                  | Problem p(f()) End.
                  |End.""".stripMargin
    val entry = parse(input).loneElement

    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Unit), Real, Some(Nil), Some("5".asTerm), UnknownLocation),
        ("p", None) -> (Some(Real), Bool, Some((("x", None), Real) :: Nil), Some(".>0".asFormula), UnknownLocation)
      )))
    entry.model shouldBe "p(f())".asFormula
    entry.expandedModel shouldBe "5>0".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse comma-separated variable declarations" in {
    val input =
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x, y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.problemContent shouldBe input
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "accept reserved identifiers" in {
    parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real Real; End.
        | Problem true End.
        |End.""".stripMargin
    ).loneElement.defs should beDecl(
      Declaration(Map(
        ("Real", None) -> (None, Real, None, None, UnknownLocation)
      ))
    )

    parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real R; End.
        | Problem true End.
        |End.""".stripMargin
    ).loneElement.defs  should beDecl(
      Declaration(Map(
        ("R", None) -> (None, Real, None, None, UnknownLocation)
      ))
    )

    parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real Bool; End.
        | Problem true End.
        |End.""".stripMargin
    ).loneElement.defs  should beDecl(
      Declaration(Map(
        ("Bool", None) -> (None, Real, None, None, UnknownLocation)
      ))
    )

    parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real HP; End.
        | Problem true End.
        |End.""".stripMargin
    ).loneElement.defs  should beDecl(
      Declaration(Map(
        ("HP", None) -> (None, Real, None, None, UnknownLocation)
      ))
    )
  }

  it should "parse a problem without variables" in {
    val input =
      """ArchiveEntry "Entry 1"
        | Definitions Real f(); End.
        | Problem f()>0 End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation)
      )))
    entry.model shouldBe "f()>0".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "allow comma-separated simple function definitions" in {
    val input =
      """ArchiveEntry "Entry 1"
        | Definitions Real f(), g; End.
        | Problem f()>g() End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("f", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation),
        ("g", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation)
      )))
    entry.model shouldBe "f()>g()".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "allow comma-separated simple predicate definitions" in {
    val input =
      """ArchiveEntry "Entry 1"
        | Definitions Bool p(), q(); End.
        | Problem p() & q() End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("p", None) -> (Some(Unit), Bool, Some(Nil), None, UnknownLocation),
        ("q", None) -> (Some(Unit), Bool, Some(Nil), None, UnknownLocation)
      )))
    entry.model shouldBe "p() & q()".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a problem that uses the allowed interpreted functions" in {
    val input =
      """ArchiveEntry "Entry 1"
        | Problem abs(-5)>0 End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs.decls shouldBe empty
    entry.model shouldBe "abs(-5)>0".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse an annotation that uses the reserved function symbol old" in {
    val input =
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; End.
        | Problem [{x:=x;}*@invariant(old(x)=x)]x=x End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "[{x:=x;}*]x=x".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a problem with neither definitions nor variables" in {
    val input =
      """ArchiveEntry "Entry 1"
        | Problem false -> true End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs.decls shouldBe empty
    entry.model shouldBe "false -> true".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a plain problem format" in {
    val input =
      """ProgramVariables Real x; Real y; End.
        |Problem x>y -> x>=y End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "<undefined>"
    entry.kind shouldBe "theorem"
    entry.problemContent shouldBe input.trim()
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a plain formula" in {
    val input = "x>y -> x>=y"
    val entry = parse(input).loneElement
    entry.name shouldBe "<undefined>"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }


  it should "refuse mixed plain and named entries" in {
    val input =
      """ProgramVariables Real x; Real y; End.
        |Problem x>y -> x>=y End.
        |
        |ArchiveEntry "Entry 2"
        |  ProgramVariables Real x; End.
        |  Problem x>0 End.
        |End.
      """.stripMargin
    val ex = the [ParseException] thrownBy parse(input)
    ex.msg should include ("Archives that start with an anonymous entry may not contain any other entries, but found ArchiveEntry")
  }

  it should "detect duplicate variable definitions" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.
      """.stripMargin
    val ex = the [ParseException] thrownBy parse(input)
    ex.msg should include ("Duplicate variable 'x'")
  }

  it should "detect duplicate function names" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Real f() = (1); Real f() = (2); End.
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.
      """.stripMargin
    val ex = the [ParseException] thrownBy parse(input)
    ex.msg should include ("Duplicate symbol 'f'")
  }

  it should "detect duplicate predicate names" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Bool p() <-> (1>0). Bool p() <-> (2>1); End.
        | ProgramVariables Real x; Real y; End.
        | Problem p() -> x>=y End.
        |End.
      """.stripMargin
    val ex = the [ParseException] thrownBy parse(input)
    ex.msg should include ("Duplicate symbol 'p'")
  }

  it should "detect duplicate program names" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions HP a ::= { ?true; }. HP a ::= { ?false; }; End.
        | ProgramVariables Real x; Real y; End.
        | Problem [a;]true End.
        |End.
      """.stripMargin
    val ex = the [ParseException] thrownBy parse(input)
    ex.msg should include ("Duplicate symbol 'a'")
  }

  it should "parse a model and tactic entry" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic "Proof 1" implyR(1) ; QE End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe ("Proof 1", "implyR(1) ; QE", implyR(1) & QE) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
    entry.problemContent shouldBe """ArchiveEntry "Entry 1"
                                    | ProgramVariables Real x; Real y; End.
                                    | Problem x>y -> x>=y End.
                                    |End.""".stripMargin.trim()
  }

  it should "parse a tactic without name" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic implyR(1) ; QE End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe ("<undefined>", "implyR(1) & QE", implyR(1) & QE) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
    entry.problemContent shouldBe
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin
  }

  it should "parse a tactic with a comment in the beginning" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic "Empty" /* a comment */ nil End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe ("Empty", "/* a comment */ nil", nil) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a pending tactic" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic "Pending" implyR(1) ; pending({`QE`}) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe ("Pending", "implyR(1) ; pending({`QE`})", implyR(1) & DebuggingTactics.pending("QE")) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a pending tactic with arguments" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic "Pending" implyR(1) ; pending({`QE({`Mathematica`}) | QE({`Z3`})`}) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe ("Pending", "implyR(1) ; pending({`QE({`Mathematica`}) | QE({`Z3`})`})", implyR(1) & DebuggingTactics.pending("QE({`Mathematica`}) | QE({`Z3`})")) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a tactic with arguments in new syntax" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> [{x'=1}]x>=y End.
        | Tactic "Simple" implyR(1) ; dC("x>=old(x)", 1) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> [{x'=1}]x>=y".asFormula
    entry.tactics shouldBe ("Simple", "implyR(1) ; dC(\"x>=old(x)\", 1)", implyR(1) & dC("x>=old(x)".asFormula)(1)) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "elaborate to functions when parsing a tactic" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Real y; End.
        | ProgramVariables Real x; End.
        | Problem x>y -> [{x'=1}]x>=y End.
        | Tactic "Simple" implyR(1) ; dC("y=old(y)", 1) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation)
      )))
    entry.model shouldBe "x>y() -> [{x'=1}]x>=y()".asFormula
    entry.tactics shouldBe ("Simple", "implyR(1) ; dC(\"y=old(y)\", 1)", implyR(1) & dC("y()=old(y())".asFormula)(1)) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "elaborate to functions in the presence of program constants" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Real y; End.
        | ProgramVariables Real x; End.
        | Problem x>y -> [ctrl;]x>=y End.
        | Tactic "Simple" implyR(1) ; dC("y=old(y)", 1) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation)
      )))
    entry.model shouldBe "x>y() -> [ctrl;]x>=y()".asFormula
    entry.tactics shouldBe ("Simple", "implyR(1) ; dC(\"y=old(y)\", 1)", implyR(1) & dC("y()=old(y())".asFormula)(1)) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "not elaborate to program constants when definitions contain duals" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | Definitions Real y(); HP ctrl ::= { x:=x+1;^@ }; End.
        | ProgramVariables Real x; End.
        | Problem x>y -> [ctrl;]x>=y End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation),
        ("ctrl", None) -> (Some(Unit), Trafo, None, Some("x:=x+1;".asProgram), UnknownLocation)
      )))
    entry.model shouldBe "x>y() -> [ctrl;]x>=y()".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a pending tactic with arguments in new syntax" in {
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> [{x'=1}]x>=y End.
        | Tactic "Simple" implyR(1) ; pending("dC(\"x>=old(x)\", 1)") End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> [{x'=1}]x>=y".asFormula
    entry.tactics shouldBe ("Simple", "implyR(1) ; pending(\"dC(\\\"x>=old(x)\\\", 1)\")", implyR(1) & DebuggingTactics.pending("dC(\\\"x>=old(x)\\\", 1)")) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a model with several tactics" in withQE { _ =>
    val input =
      """
        |ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic "Proof 1" implyR(1) & QE End.
        | Tactic "Proof 2" implyR('R) End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe ("Proof 1", "implyR(1) & QE", implyR(1) & QE) :: ("Proof 2", "implyR('R)", implyR('R)) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
    entry.problemContent shouldBe
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin
  }

  it should "parse a list of model and tactic entries" in {
    val input =
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.
        |
        |ArchiveEntry "Entry 2"
        |  Definitions Real x(); End.
        |  ProgramVariables Real y; End.
        |  Problem x()>=y -> x()>=y End.
        |  Tactic "Prop Proof" prop End.
        |End.
      """.stripMargin
    val entries = parse(input)
    entries should have size 2
    val entry1 = entries.head
    entry1.name shouldBe "Entry 1"
    entry1.kind shouldBe "theorem"
    entry1.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry1.model shouldBe "x>y -> x>=y".asFormula
    entry1.tactics shouldBe empty
    entry1.info shouldBe empty
    entry1.fileContent shouldBe """ArchiveEntry "Entry 1"
                                  | ProgramVariables Real x; Real y; End.
                                  | Problem x>y -> x>=y End.
                                  |End.""".stripMargin

    val entry2 = entries.last
    entry2.name shouldBe "Entry 2"
    entry2.kind shouldBe "theorem"
    entry2.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry2.model shouldBe "x()>=y -> x()>=y".asFormula
    entry2.tactics shouldBe ("Prop Proof", "prop", prop) :: Nil
    entry2.info shouldBe empty
    entry2.fileContent shouldBe """ArchiveEntry "Entry 2"
                                  |  Definitions Real x(); End.
                                  |  ProgramVariables Real y; End.
                                  |  Problem x()>=y -> x()>=y End.
                                  |  Tactic "Prop Proof" prop End.
                                  |End.""".stripMargin
  }

  it should "parse a list of mixed entries, lemmas, and theorems" in {
    val input =
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.
        |
        |Lemma "Entry 2"
        |  Definitions Real x(); End.
        |  ProgramVariables Real y; End.
        |  Problem x()>=y -> x()>=y End.
        |  Tactic "Prop Proof" prop End.
        |End.
        |
        |Theorem "Entry 3"
        |  ProgramVariables Real x; End.
        |  Problem x>3 -> x>=3 End.
        |End.
        |
        |ArchiveEntry "Entry 4"
        |  ProgramVariables Real x; End.
        |  Problem x>4 -> x>=4 End.
        |End.
      """.stripMargin
    val entries = parse(input)
    entries should have size 4

    val entry1 = entries.head
    entry1.name shouldBe "Entry 1"
    entry1.kind shouldBe "theorem"
    entry1.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry1.model shouldBe "x>y -> x>=y".asFormula
    entry1.tactics shouldBe empty
    entry1.info shouldBe empty
    entry1.fileContent shouldBe """ArchiveEntry "Entry 1"
                                  | ProgramVariables Real x; Real y; End.
                                  | Problem x>y -> x>=y End.
                                  |End.""".stripMargin

    val entry2 = entries(1)
    entry2.name shouldBe "Entry 2"
    entry2.kind shouldBe "lemma"
    entry2.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry2.model shouldBe "x()>=y -> x()>=y".asFormula
    entry2.tactics shouldBe ("Prop Proof", "prop", prop) :: Nil
    entry2.info shouldBe empty
    entry2.fileContent shouldBe """Lemma "Entry 2"
                                  |  Definitions Real x(); End.
                                  |  ProgramVariables Real y; End.
                                  |  Problem x()>=y -> x()>=y End.
                                  |  Tactic "Prop Proof" prop End.
                                  |End.""".stripMargin

    val entry3 = entries(2)
    entry3.name shouldBe "Entry 3"
    entry3.kind shouldBe "theorem"
    entry3.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry3.model shouldBe "x>3 -> x>=3".asFormula
    entry3.tactics shouldBe empty
    entry3.info shouldBe empty
    entry3.fileContent shouldBe """Theorem "Entry 3"
                                  |  ProgramVariables Real x; End.
                                  |  Problem x>3 -> x>=3 End.
                                  |End.""".stripMargin

    val entry4 = entries(3)
    entry4.name shouldBe "Entry 4"
    entry4.kind shouldBe "theorem"
    entry4.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry4.model shouldBe "x>4 -> x>=4".asFormula
    entry4.tactics shouldBe empty
    entry4.info shouldBe empty
    entry4.fileContent shouldBe """ArchiveEntry "Entry 4"
                                  |  ProgramVariables Real x; End.
                                  |  Problem x>4 -> x>=4 End.
                                  |End.""".stripMargin
  }

  it should "parse a list of mixed entries, lemmas, and theorems, whose names are again entry/lemma/theorem" in {
    val input =
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.
        |
        |Lemma "Lemma 2: Some Entry"
        |  Definitions Real x(); End.
        |  ProgramVariables Real y; End.
        |  Problem x()>=y -> x()>=y End.
        |  Tactic "Prop Proof of Lemma 2" prop End.
        |End.
        |
        |Theorem "Theorem 1: Some Entry"
        |  ProgramVariables Real x; End.
        |  Problem x>3 -> x>=3 End.
        |End.
        |
        |ArchiveEntry "ArchiveEntry 4: Name"
        |  ProgramVariables Real x; End.
        |  Problem x>4 -> x>=4 End.
        |End.
      """.stripMargin
    val entries = parse(input)
    entries should have size 4

    val entry1 = entries.head
    entry1.name shouldBe "Entry 1"
    entry1.kind shouldBe "theorem"
    entry1.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry1.model shouldBe "x>y -> x>=y".asFormula
    entry1.tactics shouldBe empty
    entry1.info shouldBe empty
    entry1.fileContent shouldBe """ArchiveEntry "Entry 1"
                                  | ProgramVariables Real x; Real y; End.
                                  | Problem x>y -> x>=y End.
                                  |End.""".stripMargin

    val entry2 = entries(1)
    entry2.name shouldBe "Lemma 2: Some Entry"
    entry2.kind shouldBe "lemma"
    entry2.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (Some(Unit), Real, Some(Nil), None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry2.model shouldBe "x()>=y -> x()>=y".asFormula
    entry2.tactics shouldBe ("Prop Proof of Lemma 2", "prop", prop) :: Nil
    entry2.info shouldBe empty
    entry2.fileContent shouldBe """Lemma "Lemma 2: Some Entry"
                                  |  Definitions Real x(); End.
                                  |  ProgramVariables Real y; End.
                                  |  Problem x()>=y -> x()>=y End.
                                  |  Tactic "Prop Proof of Lemma 2" prop End.
                                  |End.""".stripMargin

    val entry3 = entries(2)
    entry3.name shouldBe "Theorem 1: Some Entry"
    entry3.kind shouldBe "theorem"
    entry3.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry3.model shouldBe "x>3 -> x>=3".asFormula
    entry3.tactics shouldBe empty
    entry3.info shouldBe empty
    entry3.fileContent shouldBe """Theorem "Theorem 1: Some Entry"
                                  |  ProgramVariables Real x; End.
                                  |  Problem x>3 -> x>=3 End.
                                  |End.""".stripMargin

    val entry4 = entries(3)
    entry4.name shouldBe "ArchiveEntry 4: Name"
    entry4.kind shouldBe "theorem"
    entry4.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry4.model shouldBe "x>4 -> x>=4".asFormula
    entry4.tactics shouldBe empty
    entry4.info shouldBe empty
    entry4.fileContent shouldBe """ArchiveEntry "ArchiveEntry 4: Name"
                                  |  ProgramVariables Real x; End.
                                  |  Problem x>4 -> x>=4 End.
                                  |End.""".stripMargin
  }

  it should "parse a lemma entry" in {
    val input =
      """
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "lemma"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim()
  }

  it should "parse a theorem entry" in {
    val input =
      """
        |Theorem "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.
      """.stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe input.trim
  }

  it should "split blocks by whole word only (lemma used in tactic)" in {
    val input =
      """Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.
        |
        |Theorem "Entry 2"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic "Proof Entry 2" useLemma({`Entry 1`}) End.
        |End.""".stripMargin
    val entries = parse(input)
    entries should have size 2

    val entry1 = entries.head
    entry1.name shouldBe "Entry 1"
    entry1.kind shouldBe "lemma"
    entry1.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry1.model shouldBe "x>y -> x>=y".asFormula
    entry1.tactics shouldBe empty
    entry1.info shouldBe empty
    entry1.fileContent shouldBe
      """Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin

    val entry2 = entries(1)
    entry2.name shouldBe "Entry 2"
    entry2.kind shouldBe "theorem"
    entry2.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry2.model shouldBe "x>y -> x>=y".asFormula
    entry2.tactics shouldBe ("Proof Entry 2", "useLemma({`Entry 1`})", TactixLibrary.useLemma("Entry 1", None))::Nil
    entry2.info shouldBe empty
    entry2.fileContent shouldBe
      """Theorem "Entry 2"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic "Proof Entry 2" useLemma({`Entry 1`}) End.
        |End.""".stripMargin
  }

  it should "parse meta information" in {
    val input =
      """Lemma "Entry 1"
        | Description "The description of entry 1".
        | Title "A short entry 1 title".
        | Link "http://web.keymaerax.org/show/entry1".
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> y<x End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "lemma"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> y<x".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe Map(
      "Description" -> "The description of entry 1",
      "Title" -> "A short entry 1 title",
      "Link" -> "http://web.keymaerax.org/show/entry1")
    entry.fileContent shouldBe
      """Lemma "Entry 1"
        | Description "The description of entry 1".
        | Title "A short entry 1 title".
        | Link "http://web.keymaerax.org/show/entry1".
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> y<x End.
        |End.""".stripMargin
  }

  it should "parse meta information at any position" in {
    val input =
      """Lemma "Entry 1"
        | Description "The description of entry 1".
        | ProgramVariables Real x; Real y; End.
        | Link "http://web.keymaerax.org/show/entry1".
        | Problem x>y -> y<x End.
        | Title "A short entry 1 title".
        | Illustration "https://lfcps.org/example.png".
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "lemma"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>y -> y<x".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe Map(
      "Description" -> "The description of entry 1",
      "Title" -> "A short entry 1 title",
      "Link" -> "http://web.keymaerax.org/show/entry1",
      "Illustration" -> "https://lfcps.org/example.png"
    )
    entry.fileContent shouldBe
      """Lemma "Entry 1"
        | Description "The description of entry 1".
        | ProgramVariables Real x; Real y; End.
        | Link "http://web.keymaerax.org/show/entry1".
        | Problem x>y -> y<x End.
        | Title "A short entry 1 title".
        | Illustration "https://lfcps.org/example.png".
        |End.""".stripMargin
  }

  it should "replace tabs with spaces" in {
    // tabs throw off the position computation in the lexer. in archives, this leads to faulty tactic extraction.
    val entry = parse("ArchiveEntry \"Replace tabs\"\nProgramVariables\n\tReal x;\nEnd.\nProblem\n\tx>0\nEnd.\nTactic \"Proof\" master End. End.").loneElement
    entry.name shouldBe "Replace tabs"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>0".asFormula
    entry.tactics shouldBe ("Proof", "master", TactixLibrary.master()) :: Nil
    entry.info shouldBe empty
    entry.fileContent shouldBe
      """ArchiveEntry "Replace tabs"
        |ProgramVariables
        |  Real x;
        |End.
        |Problem
        |  x>0
        |End.
        |Tactic "Proof" master End. End.""".stripMargin
  }

  it should "replace tabs with spaces in model-only entry" in {
    // tabs throw off the position computation in the lexer (especially before \n). in archives without tactics, this leads to faulty model extraction.
    val entry = parse("ArchiveEntry \"Replace tabs\"\nProgramVariables\t\nReal x;\nEnd.\nProblem\nx>0 End. End.").loneElement
    entry.name shouldBe "Replace tabs"
    entry.kind shouldBe "theorem"
    entry.defs should beDecl(
      Declaration(Map(
        ("x", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "x>0".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe
      s"""ArchiveEntry "Replace tabs"
         |ProgramVariables${"  "}
         |Real x;
         |End.
         |Problem
         |x>0 End. End.""".stripMargin
  }

  it should "double-check extracted artifact strings" in {
    val text = """ArchiveEntry "Entry 1"
                 |ProgramVariables
                 |  Real x;
                 |End.
                 |Problem
                 |  x>0 End. End.""".stripMargin
    val tokens = KeYmaeraXLexer.inMode(text, ProblemFileMode)
    // temper with positioning
    val problemIdx = tokens.indexWhere(_.tok.img == "Problem")
    val (correctPosTokens, wrongPosTokens) = tokens.splitAt(problemIdx)
    val wrongTokens: KeYmaeraXLexer.TokenStream = correctPosTokens ++ wrongPosTokens.map(t => Token(t.tok, t.loc match {
      case Region(l, c, el, ec) => Region(l-1, c, el-1, ec)
      case l => l
    }))

    val parse = PrivateMethod[List[ParsedArchiveEntry]]('parse)
    KeYmaeraXArchiveParser invokePrivate parse(tokens, text, true) // should not fail
    the [ParseException] thrownBy (KeYmaeraXArchiveParser invokePrivate parse(wrongTokens, text, true)) should
      have message """<somewhere> Even though archive parses, extracted problem does not parse (try reformatting):
                     |ArchiveEntry "Entry 1"
                     |ProgramVariables
                     |  Real x;
                     |End.
                     |Problem
                     |Found:    <unknown> at <somewhere>
                     |Expected: <unknown>""".stripMargin
  }

  "Global definitions" should "be added to all entries" in {
    val input =
      """SharedDefinitions
        | Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.
        |
        |Theorem "Entry 2"
        | Definitions Bool geq(Real,Real) <-> ( ._0 >= ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> geq(x,y) End.
        | Tactic "Proof Entry 2" useLemma({`Entry 1`}) End.
        |End.""".stripMargin
    val entries = parse(input)
    entries should have size 2

    val entry1 = entries.head
    entry1.name shouldBe "Entry 1"
    entry1.kind shouldBe "lemma"
    entry1.defs should beDecl(
      Declaration(Map(
        ("gt", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0>._1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry1.model shouldBe "gt(x,y) -> x>=y".asFormula
    entry1.expandedModel shouldBe "x>y -> x>=y".asFormula
    entry1.tactics shouldBe empty
    entry1.info shouldBe empty
    entry1.fileContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.""".stripMargin
    entry1.problemContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.""".stripMargin

    val entry2 = entries(1)
    entry2.name shouldBe "Entry 2"
    entry2.kind shouldBe "theorem"
    entry2.defs should beDecl(
      Declaration(Map(
        ("gt", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0 > ._1".asFormula), UnknownLocation),
        ("geq", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0 >= ._1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    val foo = entry2.defs.substs
    val bar = entry2.defs.substs
    entry2.model shouldBe "gt(x,y) -> geq(x,y)".asFormula
    entry2.expandedModel shouldBe "x>y -> x>=y".asFormula
    entry2.tactics shouldBe ("Proof Entry 2", "useLemma({`Entry 1`})",
      ExpandAll(entry2.defs.substs) & TactixLibrary.useLemma("Entry 1", None))::Nil
    entry2.info shouldBe empty
    entry2.fileContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Theorem "Entry 2"
        | Definitions Bool geq(Real,Real) <-> ( ._0 >= ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> geq(x,y) End.
        | Tactic "Proof Entry 2" useLemma({`Entry 1`}) End.
        |End.""".stripMargin
  }

  it should "add to all entries but not auto-expand if tactic expands" in {
    val input =
      """SharedDefinitions
        | Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.
        |
        |Theorem "Entry 2"
        | Definitions Bool geq(Real,Real) <-> ( ._0 >= ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> geq(x,y) End.
        | Tactic "Proof Entry 2" expand "gt" ; useLemma({`Entry 1`}) End.
        |End.""".stripMargin
    val entries = parse(input)
    entries should have size 2

    val entry1 = entries.head
    entry1.name shouldBe "Entry 1"
    entry1.kind shouldBe "lemma"
    entry1.defs should beDecl(
      Declaration(Map(
        ("gt", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0>._1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry1.model shouldBe "gt(x,y) -> x>=y".asFormula
    entry1.expandedModel shouldBe "x>y -> x>=y".asFormula
    entry1.tactics shouldBe empty
    entry1.info shouldBe empty
    entry1.fileContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.""".stripMargin
    entry1.problemContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.""".stripMargin

    val entry2 = entries(1)
    entry2.name shouldBe "Entry 2"
    entry2.kind shouldBe "theorem"
    entry2.defs should beDecl(
      Declaration(Map(
        ("gt", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0 > ._1".asFormula), UnknownLocation),
        ("geq", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0 >= ._1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry2.model shouldBe "gt(x,y) -> geq(x,y)".asFormula
    entry2.expandedModel shouldBe "x>y -> x>=y".asFormula
    entry2.tactics shouldBe ("Proof Entry 2", """expand "gt" ; useLemma({`Entry 1`})""",
      Expand("gt".asNamedSymbol, "gt(._0,._1) ~> ._0 > ._1".asSubstitutionPair) &
        TactixLibrary.useLemma("Entry 1", None))::Nil
    entry2.info shouldBe empty
    entry2.fileContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Theorem "Entry 2"
        | Definitions Bool geq(Real,Real) <-> ( ._0 >= ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> geq(x,y) End.
        | Tactic "Proof Entry 2" expand "gt" ; useLemma({`Entry 1`}) End.
        |End.""".stripMargin
  }

  it should "add to all entries but not auto-expand if tactic uses US to expand" in {
    val input =
      """SharedDefinitions
        | Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.
        |
        |Theorem "Entry 2"
        | Definitions Bool geq(Real,Real) <-> ( ._0 >= ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> geq(x,y) End.
        | Tactic "Proof Entry 2" US("gt(._0,._1) ~> ._0>._1") ; useLemma({`Entry 1`}) End.
        |End.""".stripMargin
    val entries = parse(input)
    entries should have size 2

    val entry1 = entries.head
    entry1.name shouldBe "Entry 1"
    entry1.kind shouldBe "lemma"
    entry1.defs should beDecl(
      Declaration(Map(
        ("gt", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0>._1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry1.model shouldBe "gt(x,y) -> x>=y".asFormula
    entry1.expandedModel shouldBe "x>y -> x>=y".asFormula
    entry1.tactics shouldBe empty
    entry1.info shouldBe empty
    entry1.fileContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.""".stripMargin
    entry1.problemContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Lemma "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.""".stripMargin

    val entry2 = entries(1)
    entry2.name shouldBe "Entry 2"
    entry2.kind shouldBe "theorem"
    entry2.defs should beDecl(
      Declaration(Map(
        ("gt", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0 > ._1".asFormula), UnknownLocation),
        ("geq", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0 >= ._1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry2.model shouldBe "gt(x,y) -> geq(x,y)".asFormula
    entry2.expandedModel shouldBe "x>y -> x>=y".asFormula
    entry2.tactics shouldBe ("Proof Entry 2", """US("gt(._0,._1) ~> ._0>._1") ; useLemma({`Entry 1`})""",
      TactixLibrary.uniformSubstitute(RenUSubst(("gt(._0,._1)".asFormula,  "._0>._1".asFormula) :: Nil).usubst) &
        TactixLibrary.useLemma("Entry 1", None))::Nil
    entry2.info shouldBe empty
    entry2.fileContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |Theorem "Entry 2"
        | Definitions Bool geq(Real,Real) <-> ( ._0 >= ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> geq(x,y) End.
        | Tactic "Proof Entry 2" US("gt(._0,._1) ~> ._0>._1") ; useLemma({`Entry 1`}) End.
        |End.""".stripMargin
  }

  it should "not allow duplicates with local definitions" in {
    val input =
      """
        |SharedDefinitions
        | Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |
        |Lemma "Entry 1"
        | Definitions Bool gt(Real,Real) <-> ( ._0 + 0 > ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.
      """.stripMargin
    val ex = the [ParseException] thrownBy parse(input)
    ex.msg should include ("Symbol 'gt' overrides inherited definition; must declare override")
  }

  it should "not allow duplicates with local definitions even with different sorts" in {
    val input =
      """
        |SharedDefinitions
        | Bool gt(Real,Real) <-> ( ._0 > ._1 ).
        |End.
        |
        |Lemma "Entry 1"
        | Definitions Real gt(Real) = ( ._0 * 3 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> x>=y End.
        |End.
      """.stripMargin
    val ex = the [ParseException] thrownBy parse(input)
    ex.msg should include ("Symbol 'gt' overrides inherited definition; must declare override")
  }

  it should "not swallow backslashes, for example \\exists" in {
    val input =
      """SharedDefinitions
        | Bool gt(Real,Real) <-> ( \exists t (t=1 & ._0*t > ._1) ).
        |End.
        |
        |Lemma "Entry 1"
        | Definitions Bool geq(Real,Real) <-> ( ._0 >= ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> geq(x,y) End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Entry 1"
    entry.kind shouldBe "lemma"
    entry.defs should beDecl(
      Declaration(Map(
        ("gt", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("\\exists t (t=1 & ._0*t > ._1)".asFormula), UnknownLocation),
        ("geq", None) -> (Some(Tuple(Real, Real)), Bool, Some((("\\cdot", Some(0)), Real) :: (("\\cdot", Some(1)), Real) :: Nil), Some("._0 >= ._1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "gt(x,y) -> geq(x,y)".asFormula
    entry.expandedModel shouldBe "\\exists t (t=1 & x*t>y) -> x>=y".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe
      """SharedDefinitions
        |Bool gt(Real,Real) <-> ( \exists t (t=1 & ._0*t > ._1) ).
        |End.
        |Lemma "Entry 1"
        | Definitions Bool geq(Real,Real) <-> ( ._0 >= ._1 ); End.
        | ProgramVariables Real x; Real y; End.
        | Problem gt(x,y) -> geq(x,y) End.
        |End.""".stripMargin
  }

  it should "accept exercises" in {
    val input =
      """Exercise "Exercise 1"
        | Definitions Bool geq(Real a, Real b) <-> ( a >= b ); End.
        | ProgramVariables Real x, y; End.
        | Problem __________ -> geq(x,y) End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Exercise 1"
    entry.kind shouldBe "exercise"
    entry.defs should beDecl(
      Declaration(Map(
        ("geq", None) -> (Some(Tuple(Real, Real)), Bool, Some((("a", None), Real) :: (("b", None), Real) :: Nil), Some("._0 >= ._1".asFormula), UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "false".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe
      """Exercise "Exercise 1"
        | Definitions Bool geq(Real a, Real b) <-> ( a >= b ); End.
        | ProgramVariables Real x, y; End.
        | Problem __________ -> geq(x,y) End.
        |End.""".stripMargin
  }

  it should "accept exercises in definitions" in {
    val input =
      """Exercise "Exercise 1"
        | Definitions Bool geq(Real a, Real b) <-> ( __________ ); End.
        | ProgramVariables Real x, y; End.
        | Problem __________ -> geq(x,y) End.
        |End.""".stripMargin
    val entry = parse(input).loneElement
    entry.name shouldBe "Exercise 1"
    entry.kind shouldBe "exercise"
    entry.defs should beDecl(
      Declaration(Map(
        ("geq", None) -> (Some(Tuple(Real, Real)), Bool, Some((("a", None), Real) :: (("b", None), Real) :: Nil), None, UnknownLocation),
        ("x", None) -> (None, Real, None, None, UnknownLocation),
        ("y", None) -> (None, Real, None, None, UnknownLocation)
      )))
    entry.model shouldBe "false".asFormula
    entry.tactics shouldBe empty
    entry.info shouldBe empty
    entry.fileContent shouldBe
      """Exercise "Exercise 1"
        | Definitions Bool geq(Real a, Real b) <-> ( __________ ); End.
        | ProgramVariables Real x, y; End.
        | Problem __________ -> geq(x,y) End.
        |End.""".stripMargin
  }

  "Convenience wrappers" should "parse a plain formula" in {
    parser.parseAsProblemOrFormula("x>0") shouldBe "x>0".asFormula
  }

  "Archive parser error message" should "report an invalid meta info key" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | MetaInfo "Invalid key".
        | ProgramVariables Real x; End.
        | Problem x>0 End.
        |End.""".stripMargin
    ) should have message """2:2 Invalid meta info key 'MetaInfo'
                            |Found:    MetaInfo at 2:2 to 2:9
                            |Expected: Link,Citation,Title,Description,Author,See,Illustration""".stripMargin
  }

  it should "report invalid meta info value" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Title InvalidValue.
        | ProgramVariables Real x; End.
        | Problem x>0 End.
        |End.""".stripMargin
    ) should have message """2:8 Invalid meta info value
                            |Found:    InvalidValue at 2:8 to 2:19
                            |Expected: <string>""".stripMargin
  }

  it should "report missing meta info delimiter" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Title "A title"
        | ProgramVariables Real x; End.
        | Problem x>0 End.
        |End.""".stripMargin
    ) should have message """3:2 Missing meta info delimiter
                            |Found:    ProgramVariables at 3:2 to 3:17
                            |Expected: .
                            |      or: ;""".stripMargin
  }

  it should "report missing or misplaced problem blocks" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; End.
        |End.""".stripMargin
    ) should have message """3:1 Missing problem block
                            |Found:    End at 3:1 to 3:3
                            |Expected: Problem""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; End.
        | Tactic "Proof". implyR(1) End.
        |End.""".stripMargin
    ) should have message """3:2 Missing problem block
                           |Found:    Tactic at 3:2 to 3:7
                           |Expected: Problem""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; End.
        | Tactic "Proof". implyR(1) End.
        | Problem x>0 End.
        |End.""".stripMargin
    ) should have message """3:2 Misplaced problem block: problem expected before tactics
                           |Found:    Tactic at 3:2 to 3:7
                           |Expected: Problem""".stripMargin
  }

  it should "report misplaced variables or definitions" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem x>0 End.
        | ProgramVariables Real x; End.
        |End.""".stripMargin
    ) should have message """2:2 Misplaced definitions/program variables: expected before problem
                            |Found:    Problem at 2:2 to 2:8
                            |Expected: ProgramVariables""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem x>0 End.
        | Definitions Real f(); End.
        |End.""".stripMargin
    ) should have message """2:2 Misplaced definitions/program variables: expected before problem
                            |Found:    Problem at 2:2 to 2:8
                            |Expected: Definitions""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem x>0 End.
        | ProgramVariables Real x; End.
        | Definitions Real f(); End.
        |End.""".stripMargin
    ) should have message """2:2 Misplaced definitions/program variables: expected before problem
                           |Found:    Problem at 2:2 to 2:8
                           |Expected: ProgramVariables""".stripMargin
  }

  it should "report missing archive names" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry.
        | ProgramVariables Real x; End.
        | Problem x>0 End.
        |End.""".stripMargin
    ) should have message """1:13 Missing archive name
                            |Found:    . at 1:13
                            |Expected: "<string>"""".stripMargin
  }

  it should "report a missing archive entry delimiter" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem true End.""".stripMargin
    ) should have message """2:19 ArchiveEntry has no matching End.
                            |unmatched: ArchiveEntry|Lemma|Theorem|Exercise at 1:1 to 1:12--2:19 to EOF$
                            |Found:    EOF$ at 2:19 to EOF$
                            |Expected: End.
                            |Hint: Every entry (including ArchiveEntry, Problem, Lemma, Theorem, and Exercise) needs its own End. delimiter.""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem true End.
        |Theorem "Entry 2"
        | Problem false->true End.
        |End.""".stripMargin
    ) should have message """3:1 ArchiveEntry has no matching End.
                            |unmatched: ArchiveEntry|Lemma|Theorem|Exercise at 1:1 to 1:12--3:1 to 3:7
                            |Found:    Theorem at 3:1 to 3:7
                            |Expected: End.
                            |Hint: Every entry (including ArchiveEntry, Problem, Lemma, Theorem, and Exercise) needs its own End. delimiter.""".stripMargin
  }

  it should "report a missing definitions delimiter" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Real f().
        | Problem true End.
        |End.""".stripMargin
    ) should have message """3:2 Unexpected definition
                            |Found:    Problem at 3:2 to 3:8
                            |Expected: End
                            |      or: Real
                            |      or: Bool
                            |      or: HP""".stripMargin
  }

  it should "report a missing program variables delimiter" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """3:2 Unexpected program variable definition
                            |Found:    Problem at 3:2 to 3:8
                            |Expected: End
                            |      or: Real""".stripMargin
  }

  it should "report a missing problem delimiter" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem true
        | Tactic "Proof" close End.
        |End.""".stripMargin
    ) should have message """3:2 Missing problem delimiter
                            |Found:    Tactic at 3:2 to 3:7
                            |Expected: End""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem true""".stripMargin
    ) should have message """2:14 Missing problem delimiter
                           |Found:    <EOF> at 2:14 to EOF$
                           |Expected: End""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem true End""".stripMargin
    ) should have message """2:18 Missing . after delimiter End
                            |Found:    <EOF> at 2:18 to EOF$
                            |Expected: .
                            |Hint: ParseState( :+ ArchiveEntry :+ DOUBLE_QUOTES_STRING :+ MetaInfo(Map()) :+ Definitions(List(),List())  <|>  PROBLEM_BLOCK$, TRUE$, END_BLOCK$, EOF$)""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Problem true End
        |Tactic "My tactic" closeTrue End. """.stripMargin
    ) should have message """3:1 Missing . after delimiter End
                            |Found:    Tactic at 3:1 to 3:6
                            |Expected: .
                            |Hint: ParseState( :+ ArchiveEntry :+ DOUBLE_QUOTES_STRING :+ MetaInfo(Map()) :+ Definitions(List(),List())  <|>  PROBLEM_BLOCK$, TRUE$, END_BLOCK$, TACTIC_BLOCK$, DOUBLE_QUOTES_STRING, ID("closeTrue"), END_BLOCK$, PERIOD$, EOF$)""".stripMargin
  }

  it should "report parse errors in function definitions" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Real f() = 5*g() + *h(); End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:33 Unexpected token cannot be parsed
                            |Found:    * at 2:33
                            |Expected: <BeginningOfExpression>""".stripMargin
  }

  it should "report parse errors in predicate definitions" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Bool p() <-> f()+5^ > g(); End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:34 Unexpected token cannot be parsed
                            |Found:    > at 2:34
                            |Expected: <BeginningOfExpression>""".stripMargin
  }

  it should "report parse errors in program definitions" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions HP acc ::= { a:=* }; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:32 Unexpected token cannot be parsed
                            |Found:    } at 2:32
                            |Expected: ;""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions HP acc ::= { x'= }; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:31 Missing right-hand side x'=
                            |Found:    } at 2:31
                            |Expected: $$$T""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions HP acc ::= { x'=7* }; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:33 Unexpected token cannot be parsed
                            |Found:    } at 2:33
                            |Expected: <BeginningOfTerm>""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions HP acc ::= { x'=x, t'= }; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:37 Missing right-hand side t'=
                            |Found:    } at 2:37
                            |Expected: $$$T""".stripMargin
  }



  it should "report misplaced function, predicate, or program definitions" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real f(); End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:25 Function definition only allowed in Definitions block
                            |Found:    ( at 2:25
                            |Expected: ;
                            |      or: ,""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Bool p(); End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:19 Predicate and program definitions only allowed in Definitions block
                            |Found:    Bool at 2:19 to 2:22
                            |Expected: Real""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables HP a; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:19 Predicate and program definitions only allowed in Definitions block
                           |Found:    HP at 2:19 to 2:20
                           |Expected: Real""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x &; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:26 Unexpected token in ProgramVariables block
                            |Found:    & at 2:26
                            |Expected: ;
                            |      or: ,""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x Real y; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:26 Missing variable declaration delimiter
                            |Found:    Real at 2:26 to 2:29
                            |Expected: ;""".stripMargin
  }

  it should "report function definition errors" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Real f() & ; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:23 Unexpected token in function definition
                            |Found:    & at 2:23
                            |Expected: =
                            |      or: ;""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Real f() <-> 5; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:23 Function must be defined by equality
                            |Found:    <-> at 2:23 to 2:25
                            |Expected: =""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Real f() = 5!=7; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:25 Impossible elaboration: Operator PSEUDO$ expects a Term as argument but got the Formula 5!=7
                            |Found:    5!=7 at 2:25 to 2:28
                            |Expected: Term""".stripMargin
  }

  it should "report predicate definition errors" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Bool p() & ; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:23 Unexpected token in predicate definition
                            |Found:    & at 2:23
                            |Expected: <->""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Bool p() = 5>0; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:23 Predicate must be defined by equivalence
                            |Found:    = at 2:23
                            |Expected: <->""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Bool p() <-> 5+7; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:27 Impossible elaboration: Operator PSEUDO$ expects a Formula as argument but got the Term 5+7
                            |Found:    5+7 at 2:27 to 2:29
                            |Expected: Formula""".stripMargin
  }

  it should "report substitution errors" in {
    val entries = parse(
      """ArchiveEntry "Entry 1"
        | Definitions Bool p() <-> y>=0; End.
        | ProgramVariables Real y; End.
        | Problem [y:=0;]p() End.
        |End.""".stripMargin
    )
    the [ParseException] thrownBy entries.loneElement.expandedModel should have message
      """<somewhere> Definition p() as y>=0 must declare arguments {y}
        |Found:    <unknown> at <somewhere>
        |Expected: <unknown>""".stripMargin
  }

  it should "report imbalanced parentheses in predicate definitions" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Bool p() <-> ( true; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:27 Unmatched opening parenthesis in predicate definition
                            |unmatched: LPAREN$ at 2:27--2:29 to 2:32
                            |Found:    TRUE$ at 2:27 to 2:32
                            |Expected: )""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Bool p() <-> ( (true) | false; End.
        | Problem true End.
        |End.""".stripMargin
    ) should have message """2:27 Unmatched opening parenthesis in predicate definition
                            |unmatched: LPAREN$ at 2:27--2:29
                            |Found:    LPAREN$ at 2:27 to 2:29
                            |Expected: )""".stripMargin
  }

  it should "report tactic errors at the correct location" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        | Tactic "Proof 1". implyR(1) ; End.
        |End.""".stripMargin
    ) should have message """4:31 A combinator should be followed by a full tactic expression
                            |Found:    Some(BelleToken(EOF$,4:31 to EOF$)) at 4:31 to EOF$
                            |Expected: """.stripMargin
  }

  it should "report a missing entry ID separator" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry entry1 "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin
    ) should have message """1:21 Missing entry ID separator
                            |Found:    <string> at 1:21 to 1:29
                            |Expected: :""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry entry1
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin
    ) should have message """2:2 Missing entry ID separator
                            |Found:    ProgramVariables at 2:2 to 2:17
                            |Expected: :""".stripMargin
  }

  it should "report a missing entry title" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry entry1 :
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End.""".stripMargin
    ) should have message """2:2 Missing entry title
                            |Found:    ProgramVariables at 2:2 to 2:17
                            |Expected: <string>""".stripMargin
  }

  it should "report undefined entry IDs" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End entry1.""".stripMargin
    ) should have message """4:5 Archive entry ends with undefined ID entry1; define ID at entry start with ArchiveEntry entry1 : "Entry 1"
                            |Found:    <unknown> at 4:5 to 4:10
                            |Expected: <unknown>""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry entry1 : "Entry 1"
        | ProgramVariables Real x; Real y; End.
        | Problem x>y -> x>=y End.
        |End entry2.""".stripMargin
    ) should have message """4:5 Archive entry ends with ID entry2 but entry start defined entry1
                            |Found:    <unknown> at 4:5 to 4:10
                            |Expected: <unknown>""".stripMargin
  }

  it should "report sort identifier mismatches" in {
    the [ParseException] thrownBy parse(
      """ProgramVariables
        |  Real x;
        |  Rea y;
        |End.
        |Problem x>y -> x>=y End.""".stripMargin
    ) should have message """3:3 Unexpected program variable definition
                            |Found:    Rea at 3:3 to 3:5
                            |Expected: End
                            |      or: Real""".stripMargin

    the [ParseException] thrownBy parse(
      """ProgramVariables
        |  Real x;
        |  Bool y;
        |End.
        |Problem x>y -> x>=y End.""".stripMargin
    ) should have message """3:3 Predicate and program definitions only allowed in Definitions block
                            |Found:    Bool at 3:3 to 3:6
                            |Expected: Real""".stripMargin
  }

  it should "type-analyze annotations" in {
    the [ParseException] thrownBy parse(
      """Definitions
        |  Real f;
        |  Real g;
        |End.
        |
        |Problem
        |  [{?true;}*@invariant(fg > 0)]true
        |End.""".stripMargin
    ) should have message """<somewhere> type analysis: <undefined>: undefined symbol fg with index None
                            |Found:    undefined symbol at <somewhere>
                            |Expected: BaseVariable of sort Real
                            |Hint: Make sure to declare all variables in ProgramVariable and all symbols in Definitions block.""".stripMargin
  }

  it should "report program constant and differential program constant mismatches" in {
    the [ParseException] thrownBy parse(
      """ProgramVariables Real x; End.
        |Definitions HP inc ::= { x:=x+1; }; End.
        |Problem x>=0 -> [{inc}]x>=0 End.
      """.stripMargin
    ) should have message
      """<somewhere> All definitions and uses must match, but found the following mismatches:
        |Symbol 'inc{|^@|};' defined as Program, but used as DifferentialProgram in {inc}
        |Found:    {inc} at <somewhere>
        |Expected: inc{|^@|};""".stripMargin

    the [ParseException] thrownBy parse(
      """ProgramVariables Real x; End.
        |Definitions
        |  HP inc ::= { x:=x+1; };
        |  HP useInc ::= { {inc} };
        |End.
        |Problem x>=0 -> [useInc;]x>=0 End.
      """.stripMargin
    ) should have message
      """<somewhere> All definitions and uses must match, but found the following mismatches:
        |Symbol 'inc{|^@|};' defined as Program, but used as DifferentialProgram in {inc}
        |Found:    {inc} at <somewhere>
        |Expected: inc{|^@|};""".stripMargin
  }

  it should "report illegal name overloading" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions Real f(Real x) = x+1; Real f(Real x, Real y) = x+y; Bool f(Real x) <-> x>0; End.
        | Problem f(f(f(2,3))) End.
        |End.""".stripMargin
    ) should have message
      """2:66 Duplicate symbol 'f'
        |Found:    <unknown> at 2:66 to 2:88
        |Expected: <unknown>""".stripMargin

    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | Definitions HP inc ::= { x:=x+1;}; HP inc ::= { {x'=1} }; End.
        | ProgramVariables Real x; End.
        | Problem x>0 -> [inc;]x>0 End.
        |End.""".stripMargin
    ) should have message
      """2:37 Duplicate symbol 'inc'
        |Found:    <unknown> at 2:37 to 2:58
        |Expected: <unknown>""".stripMargin
  }

  it should "report variables used as functions" in {
    the [ParseException] thrownBy parse(
      """ArchiveEntry "Entry 1"
        | ProgramVariables Real x; End.
        | Problem x()>0 End.
        |End.""".stripMargin
    ) should have message
      """2:19 type analysis: Entry 1: x declared as a variable of sort Real but used as a function with arguments.
        |Found:    no arguments at 2:19 to 2:22
        |Expected: function with arguments""".stripMargin
  }


}
