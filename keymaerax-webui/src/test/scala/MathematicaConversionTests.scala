/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/
import com.wolfram.jlink.Expr
import org.scalatest._
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.tools._
import java.math.BigDecimal

import edu.cmu.cs.ls.keymaerax.Configuration
import edu.cmu.cs.ls.keymaerax.tools.qe.MathematicaConversion.{KExpr, MExpr}
import edu.cmu.cs.ls.keymaerax.tools.ext.{BaseKeYmaeraMathematicaBridge, JLinkMathematicaLink, KeYmaeraMathematicaBridge, UncheckedBaseK2MConverter, UncheckedBaseM2KConverter}
import edu.cmu.cs.ls.keymaerax.tools.qe.{K2MConverter, KeYmaeraToMathematica, MathematicaToKeYmaera}

import scala.collection.immutable._

class MathematicaConversionTests extends FlatSpec with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {

  val mathematicaConfig: Map[String, String] = Map(
      "linkName" -> Configuration(Configuration.Keys.MATHEMATICA_LINK_NAME),
      "libDir" -> Configuration(Configuration.Keys.MATHEMATICA_JLINK_LIB_DIR))

  var link: JLinkMathematicaLink = _
  var ml : KeYmaeraMathematicaBridge[KExpr] = _ //var so that we can instantiate within a test case.

  val x = Variable("x", None, Real)
  val y = Variable("y", None, Real)
  val A = Variable("A", None, Bool)
  val B = Variable("B", None, Bool)
  val xFn = Function("x", None, Real, Real)

  val zero = Number(new BigDecimal("0"))

  private val origConfig = Configuration.getOption(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS)

  def num(n : Integer) = Number(new BigDecimal(n.toString))
  def snum(n : String) = Number(new BigDecimal(n))

  override def beforeEach(): Unit = {
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "false", saveToFile = false)
  }

  override def afterEach(): Unit = {
    origConfig match {
      case Some(v) => Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, v, saveToFile = false)
      case None => Configuration.remove(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, saveToFile = false)
    }
  }

  override def beforeAll(): Unit = {
    //@note only once for the entire test suite, reduce number of Mathematica inits/shutdowns
    PrettyPrinter.setPrinter(edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXPrettyPrinter)
    link = new JLinkMathematicaLink("Mathematica")
    link.init(mathematicaConfig("linkName"), None, "false") //@todo jlink
    ml = new BaseKeYmaeraMathematicaBridge[KExpr](link, KeYmaeraToMathematica, MathematicaToKeYmaera) {}
  }

  override def afterAll(): Unit = {
    link.shutdown()
    link = null
    ml = null
  }

  private object round {
    def trip(e: KExpr, k2m: K2MConverter[KExpr] = KeYmaeraToMathematica): Unit = roundTrip(e, k2m) should be (e)

    def roundTrip(e: KExpr, k2m: K2MConverter[KExpr]): KExpr = {
      val math = k2m(e)
      ml.run(math)._2
    }
  }

  it should "convert numbers" in {
    ml.runUnchecked("2+2")._2 should be (Number(4))
  }

   "Mathematica -> KeYmaera" should "convert simple quantifiers" in {
    ml.runUnchecked("ForAll[{kyx`x}, kyx`x==kyx`x]")._2 should be (True)
    ml.runUnchecked("Exists[{kyx`x}, kyx`x==0]")._2 should be (Exists(Seq(x), Equal(x,zero)))
    ml.runUnchecked("ForAll[{kyx`x}, kyx`x==0]")._2 should be (Forall(Seq(x), Equal(x, zero)))
    //TODO-nrf polynomials?
    //TODO-nrf truth functions?
  }

  it should "convert equalities and inequalities" in {
    ml.runUnchecked("kyx`x == kyx`y")._2 should be (Equal(x, y))
    ml.runUnchecked("kyx`x == 0")._2 should be (Equal(x, zero))

    ml.runUnchecked("kyx`x != kyx`y")._2 should be (NotEqual(x, y))
    ml.runUnchecked("kyx`x != 0")._2 should be (NotEqual(x, zero))

    ml.runUnchecked("kyx`x > kyx`y")._2 should be (Greater(x, y))
    ml.runUnchecked("kyx`x > 0")._2 should be (Greater(x, zero))

    ml.runUnchecked("kyx`x >= kyx`y")._2 should be (GreaterEqual(x, y))
    ml.runUnchecked("kyx`x >= 0")._2 should be (GreaterEqual(x, zero))

    ml.runUnchecked("kyx`x < kyx`y")._2 should be (Less(x, y))
    ml.runUnchecked("kyx`x < 0")._2 should be (Less(x, zero))

    ml.runUnchecked("kyx`x <= kyx`y")._2 should be (LessEqual(x, y))
    ml.runUnchecked("kyx`x <= 0")._2 should be (LessEqual(x, zero))
  }

  it should "do math" in {
    ml.runUnchecked("2+3")._2 should be (num(5))
    ml.runUnchecked("2*3")._2 should be (num(6))
    ml.runUnchecked("6/3")._2 should be (num(2))
    ml.runUnchecked("10-5")._2 should be (num(5))
  }

  it should "not choke on rationals" in {
    ml.runUnchecked("2/5")._2 shouldBe Divide(num(2), num(5))
    ml.runUnchecked("Rational[2,5]")._2 shouldBe Divide(num(2), num(5))
  }

  it should "only convert decimal numbers and division of long arguments to rational" in {
    KeYmaeraToMathematica("0.1".asTerm).head.asString shouldBe "Rational"
    KeYmaeraToMathematica("0.1/1".asTerm).head.asString should not be "Rational"
    KeYmaeraToMathematica("0.1/1".asTerm).head.asString shouldBe "Divide"
    KeYmaeraToMathematica("1/10".asTerm).head.asString shouldBe "Rational"
  }

  it should "not choke on other reasonable numbers" in {
    ml.runUnchecked("Rationalize[0.5/10]")._2 should be (Divide(num(1),num(20)))
    ml.runUnchecked(".25/10")._2 shouldBe Number(0.025)
  }

  it should "convert arithmetic expressions correctly" in {
    ml.runUnchecked("kyx`x+kyx`y")._2 should be (Plus(x,y))
    ml.runUnchecked("kyx`x*kyx`y")._2 should be (Times(x,y))
    ml.runUnchecked("kyx`x-1")._2 should be (Plus(Number(-1), x)) //TODO-nrf these three tests are nasty.
    ml.runUnchecked("kyx`x-kyx`y")._2 should be (Plus(x, Times(Number(-1), y)))
    ml.runUnchecked("kyx`x/kyx`y")._2 should be (Times(x, Power(y,num(-1))))
    ml.runUnchecked("ForAll[{kyx`x}, kyx`x/4 == 4]")._2 shouldBe
      Forall(Seq(x),
        Equal(
          Times(
            Divide(
              num(1),
              num(4)
            ),
            x),
          num(4)
        )
      )
  }

  it should "convert rules correctly with the nonQEConverter" in {
    val localMl = new BaseKeYmaeraMathematicaBridge[KExpr](link, new  UncheckedBaseK2MConverter(), new UncheckedBaseM2KConverter()) {}
    localMl.runUnchecked("Rule[kyx`x,kyx`y]")._2 shouldBe Equal(x, y)
    localMl.runUnchecked("Rule[kyx`x[kyx`y],kyx`y]")._2 should be (Equal(FuncOf(xFn, y), y))
    localMl.runUnchecked("{{Rule[kyx`x,kyx`y]}}")._2 should be (Equal(x, y))
    localMl.runUnchecked("{{Rule[kyx`x,kyx`y], Rule[kyx`y,kyx`x]}}")._2 should be (And(Equal(x, y), Equal(y, x)))
    localMl.runUnchecked("{{Rule[kyx`x,kyx`y], Rule[kyx`y,kyx`x]}, {Rule[kyx`x[kyx`y],kyx`y]}}")._2 shouldBe
      Or(And(Equal(x, y), Equal(y, x)), Equal(FuncOf(xFn, y), y))
  }

  it should "not convert rules with the default converter" in {
    a [ConversionException] should be thrownBy ml.runUnchecked("Rule[kyx`x,kyx`y]")
    a [ConversionException] should be thrownBy ml.runUnchecked("{{Rule[kyx`x,kyx`y]}}")
    a [ConversionException] should be thrownBy ml.runUnchecked("{{Rule[kyx`x,kyx`y], Rule[kyx`y,kyx`x]}}")
  }

  it should "convert names correctly" in {
    ml.runUnchecked("kyx`x")._2 should be (x)
    ml.runUnchecked("Apply[kyx`x, {kyx`y}]")._2 should be (FuncOf(Function("x", None, Real, Real), Variable("y", None, Real)))
    ml.runUnchecked("Apply[kyx`x, {kyx`y0}]")._2 should be (FuncOf(Function("x", None, Real, Real), Variable("y0", None, Real)))
  }

  it should "convert crazy names correctly" in {
    ml.runUnchecked("kyx`x$u$")._2 should be (Variable("x_", None, Real))
    ml.runUnchecked("kyx`x[kyx`y$u$]")._2 should be (FuncOf(Function("x", None, Real, Real), Variable("y_", None, Real)))
  }

  it should "convert Boolean Algebra correctly" in {
    ml.runUnchecked("True")._2 should be (True)
    ml.runUnchecked("False")._2 should be (False)
    //These test cases are fragile because they require Mathematica to not do
    //any reduction, but Mathematica's semantics are from from clear and in
    //future versions (or previous versions) these expressions might actually
    //evaluate
    ml.runUnchecked("kyx`x==kyx`y && kyx`y==kyx`x")._2 should be (And(Equal(x,y),Equal(y,x)))
    ml.runUnchecked("kyx`x==kyx`y || kyx`y==kyx`x")._2 should be (Or(Equal(x,y),Equal(y,x)))
    ml.runUnchecked("!(kyx`x==kyx`y && kyx`y==kyx`x)")._2 should be (Not(And(Equal(x,y),Equal(y,x))))

    //ml.runUnchecked("x==y -> y==z") should be (Imply(Equals(Real,x,y),Equals(Real,y,z)))
    //ml.runUnchecked("x==y <-> y==z") should be(Equiv(Equals(Real,x,y),Equals(Real,y,z)))
  }

  it should "not fail on a grab-bag of previous errors" in {
    ml.runUnchecked("kyx`x^2 + 2kyx`x + 4")._2 shouldBe "4 + 2*x + x^2".asTerm
  }

  it should "convert inequalities" in {
    ml.runUnchecked("kyx`x < kyx`y == kyx`z < 0")._2 shouldBe "x<y & y=z & z<0".asFormula
  }

  it should "convert derivatives with the nonQEConverter" in {
    val localMl = new BaseKeYmaeraMathematicaBridge[KExpr](link, new  UncheckedBaseK2MConverter(), new UncheckedBaseM2KConverter()) {}
    localMl.runUnchecked("Derivative[1][kyx`x]")._2 shouldBe DifferentialSymbol(Variable("x"))
  }

  "KeYmaera <-> Mathematica converters" should "commute" in {
    round trip num(5)
    round trip x
    round trip Variable("y", None, Real)
    round trip Variable("xyzd", None, Real)
    //round trip Variable("_", None, Real)
    round trip Variable("x_", None, Real)
    round trip Variable("x", Some(0), Real)
    round trip Variable("x", Some(5), Real)
    round trip Variable("x_", Some(0), Real)
    round trip Variable("x_", Some(2), Real)
    round trip FuncOf(Function("x", None, Real, Real), Variable("y0", None, Real))
  }

  it should "commute crazy names" in {
    round trip Variable("x_", None, Real)
    //round trip Variable("_", None, Real)
    round trip FuncOf(Function("x", None, Real, Real), Variable("y_", None, Real))
  }

  it should "convert inequalities" in {
    round trip "\\forall x x>y".asFormula
    round trip "\\forall x x>=y".asFormula
    round trip "\\forall x x<=y".asFormula
    round trip "\\forall x x<y".asFormula
  }

  it should "associate correctly" in {
    round trip "\\forall x ((x<=y & y<=z) & z<0)".asFormula
    KeYmaeraToMathematica("5--2".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Subtract"),
      Array(new MExpr(BigInt(5).bigInteger), new MExpr(BigInt(-2).bigInteger)))
  }

  it should "convert Apply()" in {
    round trip FuncOf(Function("x", None, Real, Real), Number(0))
    round trip FuncOf(Function("x", None, Real, Real), Variable("y", None, Real))
  }

  it should "convert non-arg functions with nonQEConverter" in {
    val localMl = new BaseKeYmaeraMathematicaBridge[KExpr](link, new  UncheckedBaseK2MConverter(), new UncheckedBaseM2KConverter()) {}
    val e = "g()".asTerm
    val math = new UncheckedBaseK2MConverter()(e)
    localMl.run(math)._2 shouldBe e
  }

  it should "convert nested quantifiers" in {
    round trip "\\forall a \\forall b \\exists c \\forall d (a<=b -> c>=a+d)".asFormula
  }

  "KeYmaera -> Mathematica" should "convert Apply" in {
    val in = FuncOf(Function("y", None, Real, Real), Variable("x"))
    val expected = new MExpr(new MExpr(Expr.SYMBOL, "kyx`y"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    KeYmaeraToMathematica(in) should be (expected)
  }

  it should "convert parameterless Apply()" in {
    val in = FuncOf(Function("y", None, Unit, Real), Nothing)
    KeYmaeraToMathematica(in) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "kyx`y"), Array[MExpr]())
  }

  it should "convert variables to symbols" in {
    val in = Variable("x")
    KeYmaeraToMathematica(in) shouldBe new MExpr(Expr.SYMBOL, "kyx`x")
  }

  it should "convert multi-argument Apply to nested lists" in {
    val in = FuncOf(Function("f", None, Tuple(Real, Tuple(Real, Real)), Real), Pair(Variable("x"), Pair(Variable("y"), Variable("z"))))
    val expected = new MExpr(new MExpr(Expr.SYMBOL, "kyx`f"),
        Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x"), new MExpr(Expr.SYM_LIST, Array[MExpr](
            new MExpr(Expr.SYMBOL, "kyx`y"), new MExpr(Expr.SYMBOL, "kyx`z")))))
    println(expected.toString)
    KeYmaeraToMathematica(in) should be (expected)
  }

  it should "convert special functions only when forced to" in {
    the [CoreException] thrownBy KeYmaeraToMathematica("abs(x)".asTerm) should have message
      "Core requirement failed: Interpreted functions not allowed in soundness-critical conversion to Mathematica"
    the [CoreException] thrownBy KeYmaeraToMathematica("min(x,y)".asTerm) should have message
      "Core requirement failed: Interpreted functions not allowed in soundness-critical conversion to Mathematica"
    the [CoreException] thrownBy KeYmaeraToMathematica("max(x,y)".asTerm) should have message
      "Core requirement failed: Interpreted functions not allowed in soundness-critical conversion to Mathematica"
    the [CoreException] thrownBy KeYmaeraToMathematica("exp(x)".asTerm) should have message
      "Core requirement failed: Interpreted functions not allowed in soundness-critical conversion to Mathematica"
    the [CoreException] thrownBy KeYmaeraToMathematica("sin(x)".asTerm) should have message
      "Core requirement failed: Interpreted functions not allowed in soundness-critical conversion to Mathematica"
    the [CoreException] thrownBy KeYmaeraToMathematica("cos(x)".asTerm) should have message
      "Core requirement failed: Interpreted functions not allowed in soundness-critical conversion to Mathematica"

    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    round trip "abs(x)".asTerm
    round trip "min(x,y)".asTerm
    round trip "max(x,y)".asTerm

    round trip "exp(x)".asTerm
    round trip "sin(x)".asTerm
    round trip "cos(x)".asTerm

    KeYmaeraToMathematica("abs(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Abs"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    KeYmaeraToMathematica("min(x,y)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Min"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x"), new MExpr(Expr.SYMBOL, "kyx`y")))
    KeYmaeraToMathematica("max(x,y)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Max"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x"), new MExpr(Expr.SYMBOL, "kyx`y")))
    KeYmaeraToMathematica("exp(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Exp"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    KeYmaeraToMathematica("sin(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Sin"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    KeYmaeraToMathematica("cos(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Cos"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
  }

  it should "distinguish uninterpreted names from interpreted ones by namespace" in {
    Configuration.set(Configuration.Keys.QE_ALLOW_INTERPRETED_FNS, "true", saveToFile = false)
    
    KeYmaeraToMathematica(Variable("abs")) shouldBe new MExpr(Expr.SYMBOL, "kyx`abs")
    KeYmaeraToMathematica(Variable("Abs")) shouldBe new MExpr(Expr.SYMBOL, "kyx`Abs")
    a [CoreException] should be thrownBy KeYmaeraToMathematica("abs()".asTerm)
    KeYmaeraToMathematica("Abs(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "kyx`Abs"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    KeYmaeraToMathematica("abs(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Abs"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    a [CoreException] should be thrownBy KeYmaeraToMathematica("abs(x,y)".asTerm)
    
    KeYmaeraToMathematica(Variable("min")) shouldBe new MExpr(Expr.SYMBOL, "kyx`min")
    KeYmaeraToMathematica(Variable("Min")) shouldBe new MExpr(Expr.SYMBOL, "kyx`Min")
    a [CoreException] should be thrownBy KeYmaeraToMathematica("min(x)".asTerm)
    KeYmaeraToMathematica("Min(x,y)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "kyx`Min"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x"), new MExpr(Expr.SYMBOL, "kyx`y")))
    KeYmaeraToMathematica("min(x,y)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Min"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x"), new MExpr(Expr.SYMBOL, "kyx`y")))
    a [CoreException] should be thrownBy KeYmaeraToMathematica("min(x,y,z)".asTerm)
    
    KeYmaeraToMathematica(Variable("max")) shouldBe new MExpr(Expr.SYMBOL, "kyx`max")
    KeYmaeraToMathematica(Variable("Max")) shouldBe new MExpr(Expr.SYMBOL, "kyx`Max")
    a [CoreException] should be thrownBy KeYmaeraToMathematica("max(x)".asTerm)
    KeYmaeraToMathematica("Max(x,y)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "kyx`Max"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x"), new MExpr(Expr.SYMBOL, "kyx`y")))
    KeYmaeraToMathematica("max(x,y)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Max"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x"), new MExpr(Expr.SYMBOL, "kyx`y")))
    a [CoreException] should be thrownBy KeYmaeraToMathematica("max(x,y,z)".asTerm)
    
    KeYmaeraToMathematica(Variable("exp")) shouldBe new MExpr(Expr.SYMBOL, "kyx`exp")
    KeYmaeraToMathematica(Variable("Exp")) shouldBe new MExpr(Expr.SYMBOL, "kyx`Exp")
    a [CoreException] should be thrownBy KeYmaeraToMathematica("exp()".asTerm)
    KeYmaeraToMathematica("Exp(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "kyx`Exp"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    KeYmaeraToMathematica("exp(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Exp"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    a [CoreException] should be thrownBy KeYmaeraToMathematica("exp(x,y)".asTerm)

    KeYmaeraToMathematica(Variable("sin")) shouldBe new MExpr(Expr.SYMBOL, "kyx`sin")
    KeYmaeraToMathematica(Variable("Sin")) shouldBe new MExpr(Expr.SYMBOL, "kyx`Sin")
    a [CoreException] should be thrownBy KeYmaeraToMathematica("sin()".asTerm)
    KeYmaeraToMathematica("Sin(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "kyx`Sin"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    KeYmaeraToMathematica("sin(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Sin"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    a [CoreException] should be thrownBy KeYmaeraToMathematica("sin(x,y)".asTerm)
    
    KeYmaeraToMathematica(Variable("cos")) shouldBe new MExpr(Expr.SYMBOL, "kyx`cos")
    KeYmaeraToMathematica(Variable("Cos")) shouldBe new MExpr(Expr.SYMBOL, "kyx`Cos")
    a [CoreException] should be thrownBy KeYmaeraToMathematica("cos()".asTerm)
    KeYmaeraToMathematica("Cos(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "kyx`Cos"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    KeYmaeraToMathematica("cos(x)".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Cos"), Array[MExpr](new MExpr(Expr.SYMBOL, "kyx`x")))
    a [CoreException] should be thrownBy KeYmaeraToMathematica("cos(x,y)".asTerm)
  }

  it should "convert decimal to rational" in {
    KeYmaeraToMathematica("0.1".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Rational"), Array(new MExpr(BigInt(1).bigInteger), new MExpr(BigInt(10).bigInteger)))
    MathematicaToKeYmaera(KeYmaeraToMathematica("0.1".asTerm) ) shouldBe "1/10".asTerm
    KeYmaeraToMathematica("1.5".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Rational"), Array(new MExpr(BigInt(15).bigInteger), new MExpr(BigInt(10).bigInteger)))
    MathematicaToKeYmaera(KeYmaeraToMathematica("1.5".asTerm) ) shouldBe "15/10".asTerm
    KeYmaeraToMathematica("3.033".asTerm) shouldBe new MExpr(new MExpr(Expr.SYMBOL, "Rational"), Array(new MExpr(BigInt(3033).bigInteger), new MExpr(BigInt(1000).bigInteger)))
    MathematicaToKeYmaera(KeYmaeraToMathematica("3.033".asTerm) ) shouldBe "3033/1000".asTerm
  }

  it should "convert non-long numbers to big ints" in {
    KeYmaeraToMathematica(Number(Long.MaxValue)) shouldBe new MExpr(BigInt(Long.MaxValue).bigInteger)
    KeYmaeraToMathematica(Number(Long.MinValue)) shouldBe new MExpr(BigInt(Long.MinValue).bigInteger)
    KeYmaeraToMathematica(Number(Number(Long.MaxValue).value + 1)) shouldBe new MExpr((BigInt(Long.MaxValue) + 1).bigInteger)
    KeYmaeraToMathematica(Number(Number(Long.MinValue).value - 1)) shouldBe new MExpr((BigInt(Long.MinValue) - 1).bigInteger)
    KeYmaeraToMathematica(Number(Double.MaxValue)) shouldBe new MExpr(scala.BigDecimal(Double.MaxValue).toBigInt().bigInteger)
  }
}
