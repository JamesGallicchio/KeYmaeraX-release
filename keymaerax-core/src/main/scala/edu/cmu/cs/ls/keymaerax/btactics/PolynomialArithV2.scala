package edu.cmu.cs.ls.keymaerax.btactics

import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.btactics.TactixLibrary._
import edu.cmu.cs.ls.keymaerax.btactics.TacticFactory._
import edu.cmu.cs.ls.keymaerax.btactics.Idioms._
import edu.cmu.cs.ls.keymaerax.btactics.SimplifierV3._
import edu.cmu.cs.ls.keymaerax.btactics.AnonymousLemmas._
import edu.cmu.cs.ls.keymaerax.infrastruct._
import edu.cmu.cs.ls.keymaerax.infrastruct.{RenUSubst, SubstitutionHelper}
import edu.cmu.cs.ls.keymaerax.infrastruct.Augmentors._
import edu.cmu.cs.ls.keymaerax.bellerophon._
import edu.cmu.cs.ls.keymaerax.pt.ProvableSig
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.tools.qe.BigDecimalQETool

import scala.collection.immutable._

/**
  * Polynomial Arithmetic.
  *
  * Computations are carried out fairly efficiently in a distributive representation.
  * Computations are certifying:
  *   - the internal data structures maintain a proof that the constructed term equals the distributive representation
  *
  * The main interface is that of a [[PolynomialRing]]
  *
  * @author Fabian Immler
  */
object PolynomialArithV2 {

  /**
    * Polynomial Ring in a given sequence of [[variables]] (e.g., Seq(x1, ..., xn)).
    *
    * - interface that describes [[Polynomial]]s and operations on them
    * - constructors for Polynomials from constant numbers, variables, and recursively from terms
    * */
  trait PolynomialRing {

    val variables: IndexedSeq[Term]

    /**
      * Interface to [[Polynomial]]s:
      * - a [[term]] that keeps track of how the polynomial was constructed
      * - a proof for the internal [[representation]] of the polynomial
      * - arithmetic
      * - test for zero
      * */
    trait Polynomial {
      val term: Term
      // proof of "term = some internal representation"
      def representation: ProvableSig
      // proof of "term = some internal representation"
      def prettyRepresentation: ProvableSig
      // resetTerm.term = some internal representation
      def resetTerm : Polynomial

      // result.term = term + other.term
      def +(other: Polynomial) : Polynomial

      // result.term = term - other.term
      def -(other: Polynomial) : Polynomial

      // result.term = term * other.term
      def *(other: Polynomial) : Polynomial

      // result.term = -term
      def unary_- : Polynomial

      // result.term = term ^ n
      def ^(n: Int) : Polynomial

      // Some(proof of "term = 0")
      def zeroTest : Option[ProvableSig]

      // partition monomials (where (num, denum, powers) represents num/denum*(vars(i)^powers(i))_(i))
      // partition(P) = (proof of "term = p1.term + p2.term", p1, p2)
      //   where p1's monomials satisfy P and p2's monomials satisfy !P
      def partition(P: (BigDecimal, BigDecimal, IndexedSeq[Int]) => Boolean) : (Polynomial, Polynomial, ProvableSig)

      // approx(prec) = (proof of "term = p1.term + p2.term", p1, p2)
      //   where the coefficients p1 are rounded to (decimal) precision [[prec]]
      def approx(prec: Int) : (ProvableSig, Polynomial, Polynomial)

    }

    // result.term = n
    def Const(n: BigDecimal) : Polynomial

    // result.term = num/denum
    def Const(num: BigDecimal, denum: BigDecimal) : Polynomial

    // result.term = variables(index) ^ n
    def Var(index: Int, n: Int) : Polynomial

    // result.term = variables(index)
    def Var(index: Int) : Polynomial

    private val indices = variables.zipWithIndex.toMap

    // result.term = t
    def ofTerm(t: Term) : Polynomial = t match {
      case Plus(a, b)  => ofTerm(a) + ofTerm(b)
      case Minus(a, b) => ofTerm(a) - ofTerm(b)
      case Times(a, b) => ofTerm(a) * ofTerm(b)
      case Neg(a)      => -ofTerm(a)
      case Power(a, Number(i)) if i.isValidInt && i >= 0 => ofTerm(a) ^ i.toIntExact
      case Divide(Number(num), Number(denum)) if denum != 0 => Const(num, denum)
      case Number(n) => Const(n)
      case t => indices.get(t) match {
        case Some(i) => Var(i)
        case None => throw new IllegalArgumentException("Term not element of variables\nterm = " + t + "\nvariables = " + variables)
      }
    }

    implicit def ofInt(i: Int) : Polynomial = Const(BigDecimal(i))
  }

  /** construct a Polynomial ring in a given sequence of variables */
  def PolynomialRing(variables: IndexedSeq[Term]) : PolynomialRing = TwoThreeTreePolynomialRing(variables)

  /** Test for zero, returns a provable if "term = 0" can be proved using Polynomial arithmetic.
    * Computes in a polynomial ring in all non-polynomial-arithmetic subterms.
    * */
  def isZero(term: Term) : Option[ProvableSig] = {
    def symbols(t: Term) : Seq[Term] = t match {
      case Plus(a, b)  => symbols(a) ++ symbols(b)
      case Minus(a, b) => symbols(a) ++ symbols(b)
      case Times(a, b) => symbols(a) ++ symbols(b)
      case Neg(a)      => symbols(a)
      case Power(a, Number(i)) if i.isValidInt && i >= 0 => symbols(a)
      case Divide(Number(num), Number(denum)) if denum != 0 => Seq()
      case Number(n) => Seq()
      case t => Seq(t)
    }
    PolynomialRing(symbols(term).distinct.toIndexedSeq).ofTerm(term).zeroTest
  }

}

object PolynomialArithV2Helpers {
  // TODO: move somewhere reasonable
  def constR(name: String) = FuncOf(Function(name, None, Unit, Real), Nothing)
  def anyR(name: String) = UnitFunctional(name, AnyArg, Real)

  // @note: exposing private methods, only for tests
  def usePrvAt(fact: ProvableSig, key: PosInExpr): DependentPositionTactic = useAt(fact, key)
  def usePrvFor(fact: ProvableSig, key: PosInExpr): ForwardPositionTactic = useFor(fact, key)

  def substAny(s: String, t: Term) = USubst(Seq(SubstitutionPair(anyR(s), t)))

  def anyArgify(prv: ProvableSig) = {
    require(prv.isProved)
    val us = USubst(StaticSemantics.signature(prv.conclusion).flatMap{
      case f@Function(n, None, Unit, Real, false) => Some(SubstitutionPair(FuncOf(f, Nothing), UnitFunctional(n, AnyArg, Real)))
      case _ => None
    }.toIndexedSeq)
    prv(us)
  }

  val equalReflex = anyArgify(DerivedAxioms.equalReflex.fact)
  val spat = "s_(||)".asTerm
  def equalReflex(t: Term) : ProvableSig = equalReflex(USubst(Seq(SubstitutionPair(spat, t))))

  def substOfInst(inst: Seq[(String, Term)]) = USubst(inst.map{case(a, b) => SubstitutionPair(anyR(a), b)})
  def useDirectly(prv: ProvableSig, inst: Seq[(String, Term)], assms: Seq[ProvableSig]) : ProvableSig = {
    val prv2 = prv(substOfInst(inst))
    impliesElim(prv2, assms)
  }
  def useDirectlyConst(prv: ProvableSig, inst: Seq[(String, Term)], assms: Seq[ProvableSig]) : ProvableSig = {
    val prv2 = prv(USubst(inst.map { case (a, b) => SubstitutionPair(constR(a), b) }))
    impliesElim(prv2, assms)
  }

  // G |- P->Q   G |- P
  // ---------
  // G |- Q
  def impliesElim(PQ: ProvableSig, P: ProvableSig) : ProvableSig = {
    require(PQ.isProved)
    require(P.isProved)
    require(PQ.conclusion.succ.length == 1)
    require(P.conclusion.succ.length == 1)
    val pq = PQ.conclusion.succ(0)
    val p = P.conclusion.succ(0)
    pq match {
      case Imply(pp, q) => ProvableSig.startProof(Sequent(PQ.conclusion.ante, IndexedSeq(q)))(CutRight(p, SuccPos(0)), 0)(PQ, 1)(P, 0)
      case _ => throw new IllegalArgumentException("Cannot impliesElim here")
    }
  }

  /**
    * PsQ: G |- (p1 & ... & pn) -> q
    * Ps: G |- p1, ... G |- pn
    * @return G |- q
    * */
  def impliesElim(PsQ: ProvableSig, Ps: Seq[ProvableSig]) : ProvableSig =
    if (Ps.length == 0) PsQ
    else {
      val conj = Ps.map(P => P.conclusion.succ(0)).reduceRight(And)
      val conjPrv = Ps.dropRight(1).foldLeft(ProvableSig.startProof(Sequent(PsQ.conclusion.ante, IndexedSeq(conj)))){(prv, P) =>
        prv(AndRight(SuccPos(0)), 0)(P, 0)
      }(Ps.last, 0)
      impliesElim(PsQ, conjPrv)
    }

  def rememberAny(fml: Formula, be: BelleExpr) = anyArgify(remember(fml, be).fact)

  def byExact(assm: ProvableSig) = "byExact" by { (prv: ProvableSig, pos: SuccPosition) =>
    assert(prv.subgoals.length==1, "require one subgoal byExact")
    prv.apply(assm, 0)
  }

  def rhsOf(prv: ProvableSig) = prv.conclusion.succ(0).asInstanceOf[Equal].right
  def lhsOf(prv: ProvableSig) = prv.conclusion.succ(0).asInstanceOf[Equal].left

}

/**
* A polynomial is represented as a set of monomials stored in a 2-3 Tree, the ordering is lexicographic
* A monomial is represented as a coefficient and a power-product.
* A coefficient is represented as a pair of BigDecimals for num/denum.
* A power product is represented densely as a list of exponents
*
* All data-structures maintain a proof of
*  input term = representation of data structure as Term
*
* Representations of data structures (recursively applied on rhs):
*   - 3-Node (l, v1, m, v2, r) is "l + v1 + m + v2 + r"
*   - 2-Node (l, v, r) is "l + v + r"
*   - monomial (c, pp) is "c * pp"
*   - coefficient (num, denum) is "num / denum"
*   - power product [e1, ..., en] is "x1^e1 * ... * xn ^ en",
*     where instead of "x^0", we write "1" in order to avoid trouble with 0^0, i.e., nonzero-assumptions on x or the like
*
* All operations on the representations update the proofs accordingly.
*
*/
case class TwoThreeTreePolynomialRing(variables: IndexedSeq[Term]) extends PolynomialArithV2.PolynomialRing {
  import PolynomialArithV2Helpers._


  val constL = constR("l_")
  val constR_ = constR("r_")
  val constCl = constR("cl_")
  val constCr = constR("cr_")
  val constC = constR("c_")


  val constLn = constR("ln_")
  val constLd = constR("ld_")
  val constRn = constR("rn_")
  val constRd = constR("rd_")
  val coefficientTimesPrv = rememberAny(
    ("(l_() = ln_()/ld_() & r_() = rn_()/rd_() & ((ln_()*rn_() = pn_() & ld_()*rd_()=pd_() & ld_() != 0 & rd_() != 0)<-> true)) ->" +
      "l_()*r_() = pn_()/pd_()").asFormula, QE & done)
  val coefficientPlusPrv = rememberAny(
    ("(l_() = ln_()/ld_() & r_() = rn_()/rd_() & ((ln_()*rd_() + rn_()*ld_() = pn_() & ld_()*rd_()=pd_() & ld_() != 0 & rd_() != 0)<-> true)) ->" +
      "l_()+r_() = pn_()/pd_()").asFormula, QE & done)
  val coefficientNegPrv = rememberAny(
    ("(x_() = xn_()/xd_() & ((-xn_()=nxn_() & xd_() != 0)<-> true)) ->" +
      "-x_() = nxn_()/xd_()").asFormula, QE & done)

  /**
  * prv: lhs = rhs
  * lhs: input term (arbitrary, trace of construction)
  * rhs: Divide(Number(num), Number(denum))
  */
  case class Coefficient(num: BigDecimal, denum: BigDecimal,
                         prvO: Option[ProvableSig] = None) {
    val numN = Number(num)
    val denumN = Number(denum)
    // @note detour for "dependent" default argument
    lazy val defaultPrv = equalReflex(Divide(numN, denumN))
    val prv = prvO.getOrElse(defaultPrv)
    def forgetPrv = Coefficient(num, denum, Some(defaultPrv))
    def rhsString = if (num.compareTo(0) == 0) "0"
    else if (denum.compareTo(1) == 0) num.toString
    else num.toString + "/" + denum.toString

    assert(prv.subgoals.isEmpty)
    assert(prv.conclusion.ante.isEmpty)
    assert(prv.conclusion.succ.length==1)
    assert(prv.conclusion.succ(0) match {
      case Equal(lhs, Divide(Number(n), Number(d))) => n == num && d == denum
      case _ => false
    })
    val (eq, lhs, rhs) = prv.conclusion.succ(0) match { case eq @ Equal(lhs, rhs@Divide(n, d)) => (eq, lhs, rhs) }

    def unary_- : Coefficient = {
      val negPrv = ProvableSig.proveArithmetic(BigDecimalQETool, And(Equal(Neg(numN), Number(-num)), NotEqual(denumN, Number(0))))
      Coefficient(-num, denum, Some(useDirectly(coefficientNegPrv,
        Seq(
          ("x_", lhs),
          ("xn_", numN),
          ("xd_", denumN),
          ("nxn_", Number(-num))
        ),
        Seq(prv, negPrv)
      )))
    }

    def +(that: Coefficient) : Coefficient = {
      val numRes = num*that.denum + that.num*denum
      val denumRes = denum*that.denum
      val inst = Seq(
        ("ln_", numN),
          ("ld_", denumN),
          ("rn_", that.numN),
          ("rd_", that.denumN),
          ("l_", lhs),
          ("r_", that.lhs),
          ("pn_", Number(numRes)),
          ("pd_", Number(denumRes)))
      val numericPrv = ProvableSig.proveArithmetic(BigDecimalQETool,
        List(
          Equal(Plus(Times(numN, that.denumN), Times(that.numN, denumN)), Number(numRes)),
          Equal(Times(denumN, that.denumN), Number(denumRes)),
          NotEqual(denumN, Number(0)),
          NotEqual(that.denumN, Number(0)),
        ).reduceRight(And)
      )
      val prvRes = useDirectly(coefficientPlusPrv, inst, Seq(prv, that.prv, numericPrv))
      Coefficient(numRes, denumRes, Some(prvRes))
    }

    def *(that: Coefficient) : Coefficient = {
      val numRes = num*that.num
      val denumRes = denum*that.denum
      val inst = Seq(
        ("ln_", numN),
          ("ld_", denumN),
          ("rn_", that.numN),
          ("rd_", that.denumN),
          ("l_", lhs),
          ("r_", that.lhs),
          ("pn_", Number(numRes)),
          ("pd_", Number(denumRes)))
      val numericPrv = ProvableSig.proveArithmetic(BigDecimalQETool,
        List(
          Equal(Times(numN, that.numN), Number(numRes)),
          Equal(Times(denumN, that.denumN), Number(denumRes)),
          NotEqual(denumN, Number(0)),
          NotEqual(that.denumN, Number(0)),
        ).reduceRight(And)
      )
      val prvRes = useDirectly(coefficientTimesPrv, inst, Seq(prv, that.prv, numericPrv))
      Coefficient(numRes, denumRes, Some(prvRes))
    }

    /** normalized to a nicer output form, i.e., simplify rhs with
      *   0 / d = 0
      *   n / 1 = n
      * */
    def normalized : (ProvableSig, Term) = if (num.compareTo(0) == 0) {
      (useDirectly(normalizeCoeff0, Seq(("c_", lhs), ("d_", denumN)), Seq(prv)), Number(0))
    } else if (denum.compareTo(1) == 0) {
      (useDirectly(normalizeCoeff1, Seq(("c_", lhs), ("n_", numN)), Seq(prv)), numN)
    } else {
      (prv, rhs)
    }

    def split(newNum: BigDecimal, newDenum: BigDecimal) : (ProvableSig, Coefficient, Coefficient) = {
      val num1 = newNum
      val denum1 = newDenum
      val num2 = num * denum1 - num1 * denum
      val denum2 = denum * denum1
      val numericCondition = ProvableSig.proveArithmetic(BigDecimalQETool,
        splitCoefficientNumericCondition(numN, denumN, Number(num1), Number(denum1), Number(num2), Number(denum2)))
      (useDirectly(splitCoefficient, Seq(("c_", lhs), ("n_", numN), ("d_", denumN),
        ("n1_", Number(num1)), ("d1_", Number(denum1)),
        ("n2_", Number(num2)), ("d2_", Number(denum2)),
      ), Seq(prv, numericCondition)),
        Coefficient(num1, denum1), Coefficient(num2, denum2))
    }

    def approx(prec: Int) : (ProvableSig, Coefficient, Coefficient) = {
      val (l, _) = IntervalArithmeticV2.eval_ivl(prec-1)(HashMap(), HashMap())(rhs)
      split(l, 1) // @note: this is round to negative infinity - does it matter?
    }

  }

  val identityTimes = rememberAny("1*f_() = f_()".asFormula, QE & done)
  val timesIdentity = rememberAny("f_()*1 = f_()".asFormula, QE & done)

  val plusTimes = rememberAny("l_() = a_()*b_() & r_() = c_()*b_() & a_() + c_() = d_() -> l_() + r_() = d_()*b_()".asFormula, QE & done)
  val negTimes = rememberAny("l_() = a_()*b_() & -a_() = c_() -> -l_() = c_()*b_()".asFormula, QE & done)

  private val maxDegree = 20
  private def powerLemmaFormula(i: Int, j: Int) = {
    val x = constR("x_")
    Equal(Times(Power(x, Number(i)), Power(x, Number(j))), Power(x, Number(i + j)))
  }
  val powerLemmaCache =
    (for (i <- 1 to maxDegree; j <- 1 to maxDegree) yield
      ((i, j), rememberAny(powerLemmaFormula(i, j), QE & done))).toMap

  def powerLemma(i: Int, j: Int) = powerLemmaCache.get((i, j)).getOrElse(
    ??? // todo: could do this: rememberAny(powerLemma(i, j), QE & done), but keeping the error to catch potential performance issues when calling QE?
  )
  private def mkConstN(s: String, i: Int) = s + i.toString + "_"
  private def mkConst(s: String, i: Int) = FuncOf(Function(mkConstN(s, i), None, Unit, Real), Nothing)

  /**
    * l = cl * xls
    * r = cr * xrs
    * c = cl*cr
    * xs = xls ** xrs
    * ->
    * l*r=c*xs
    * */
  val monomialTimesLemma = {
    val l = constL
    val cl = constCl
    def xl(i: Int) = mkConst("xl", i)
    val xls = (0 until variables.length).map(xl)
    val r = constR_
    val cr = constCr
    def xr(i: Int) = mkConst("xr", i)
    val xrs = (0 until variables.length).map(xr)

    val c = mkConst("c", 0)
    def x(i: Int) = mkConst("x", i)
    val xs = (0 until variables.length).map(x)

    val powersAssm = (0 until variables.length).map(i => Equal(Times(xl(i), xr(i)), x(i))).reduceRight(And)
    val assms = Seq(
      Equal(l, Times(cl, xls.reduceLeft(Times))),
      Equal(r, Times(cr, xrs.reduceLeft(Times))),
      Equal(Times(cl, cr), c),
      powersAssm).reduceRight(And)
    val concl = Equal(Times(l, r), Times(c, xs.reduceLeft(Times)))
    rememberAny(Imply(assms, concl), QE & done)
  }

  def variablePower(powers: Int => Int)(i: Int) = {
    val p = powers(i)
    if (p > 0) Power(variables(i), Number(p)) else Number(1)
  }

  val constF = anyR("f_")
  val constX = anyR("x_")

  /**
    * prv: lhs = rfhs
    * lhs: input term (arbitrary, trace of construction)
    * rhs: representation of `coeff*vars^powers`
    * */
  case class Monomial(coeff: Coefficient, powers: IndexedSeq[Int], prvO: Option[ProvableSig] = None) extends Ordered[Monomial] {

    def powersTerm: Term = (0 until variables.length).map(variablePower(powers)).reduceLeft(Times)

    def monomialTerm(coeff: Term): Term = Times(coeff, powersTerm)

    def powersString: String = {
      val sep = " " // nicer than "*" ?
      (if (coeff.num.compareTo(1) == 0 && coeff.denum.compareTo(1) == 0 && powers.exists(_ > 0)) ""
      else if (coeff.num.compareTo(-1) == 0 && coeff.denum.compareTo(1) == 0) "-"
      else coeff.rhsString + sep) +
        (0 until variables.length).flatMap(i => if (powers(i) > 0) Some(Power(variables(i), Number(powers(i)))) else None).mkString(sep)
    }

    lazy val defaultPrv = equalReflex(monomialTerm(coeff.rhs))

    def forgetPrv = Monomial(coeff, powers, Some(defaultPrv))

    // @note detour for "dependent" default argument
    val prv = prvO.getOrElse(defaultPrv)

    // @todo: finish proof!
    //assert(prv.subgoals.isEmpty)
    assert(prv.conclusion.ante.isEmpty)
    assert(prv.conclusion.succ.length == 1)
    assert(prv.conclusion.succ(0) match {
      case Equal(_, rhs) => rhs == monomialTerm(coeff.rhs)
      case _ => false
    })
    val (eq, lhs, rhs) = prv.conclusion.succ(0) match {
      case eq@Equal(lhs, rhs@(Times(_, _))) => (eq, lhs, rhs)
    }

    private def solvePowers(prv: ProvableSig, i: Int): ProvableSig = {
      prv.subgoals(i).succ(0) match {
        case And(_, _) =>
          solvePowers(solvePowers(prv(AndRight(SuccPos(0)), i), i + 1), i)
        case Equal(Times(Number(_), f), _) =>
          // 1 * f = f
          prv(identityTimes(USubst(SubstitutionPair(constF, f) :: Nil)), i)
        case Equal(Times(f, Number(_)), _) =>
          // f * 1 = f
          prv(timesIdentity(USubst(SubstitutionPair(constF, f) :: Nil)), i)
        case Equal(Times(Power(x, Number(n)), Power(_, Number(m))), _) =>
          // _^i * _^j = _^(i+j)
          prv(powerLemma(n.toIntExact, m.toIntExact)(USubst(SubstitutionPair(constX, x) :: Nil)), i)
        case e =>
          ???
      }
    }

    def *(that: Monomial): Monomial = {
      val newCoeff = coeff.forgetPrv * that.coeff.forgetPrv
      val newPowers = (powers, that.powers).zipped.map(_ + _)
      // TODO: just use a match for simplicity?
      val inst = Seq(("l_", lhs),
        ("r_", that.lhs),
        ("cl_", coeff.rhs),
        ("cr_", that.coeff.rhs),
        ("c0_", newCoeff.rhs)
      ) ++
        (0 until variables.length).map(i => (mkConstN("xl", i), variablePower(powers)(i))) ++
        (0 until variables.length).map(i => (mkConstN("xr", i), variablePower(that.powers)(i))) ++
        (0 until variables.length).map(i => (mkConstN("x", i), variablePower(newPowers)(i)))
      val monomialTimesLemmaInst = monomialTimesLemma(substOfInst(inst))
      val powersFml = monomialTimesLemmaInst.conclusion.succ(0) match {
        case Imply(And(_, And(_, And(_, powersFml))), _) => powersFml
        case _ => throw new RuntimeException("powersAssm???")
      }
      val powersPrv = solvePowers(ProvableSig.startProof(powersFml), 0)
      val newPrv = impliesElim(monomialTimesLemmaInst, Seq(prv, that.prv, newCoeff.prv, powersPrv))
      Monomial(newCoeff, newPowers, Some(newPrv))
    }

    def unary_- : Monomial = {
      val newCoeff = -(coeff.forgetPrv)
      val newPrv = useDirectly(negTimes, Seq(("l_", lhs), ("a_", coeff.rhs), ("b_", rhs.right), ("c_", newCoeff.rhs)),
        Seq(prv, newCoeff.prv))
      Monomial(newCoeff, powers, Some(newPrv))
    }

    // TODO: weird signature for addition...
    def +(that: Monomial): Option[Monomial] = if (that.powers == powers) Some {
      val newCoeff = coeff.forgetPrv + that.coeff.forgetPrv

      val inst = Seq(("l_", lhs), ("r_", that.lhs), ("a_", coeff.rhs), ("b_", rhs.right), ("c_", that.coeff.rhs), ("d_", newCoeff.rhs))
      val newPrv = useDirectly(plusTimes, inst, Seq(prv, that.prv, newCoeff.prv))
      Monomial(newCoeff, powers, Some(newPrv))
    } else None

    // reverse lexicographic ordering, TODO: why not thats?
    override def compare(that: Monomial): Int = {
      val l = variables.length

      def compareAt(i: Int): Int =
        if (i >= l) 0
        else {
          val c = powers(i).compare(that.powers(i))
          if (c == 0) compareAt(i + 1)
          else c
        }
      // note *reverse*
      -compareAt(0)
    }

    def normalizePowers(c: Coefficient, t: Term): (ProvableSig, Term) = t match {
      case Times(ps, Number(n)) =>
        //assert((n.compareTo(1) == 0))
        val (cpsPrv, cps) = normalizePowers(c, ps)
        (useDirectly(normalizePowersR1, Seq(("c_", c.lhs), ("ps_", ps), ("cps_", cps)), Seq(cpsPrv)), cps)
      case Times(ps, t@Power(v, Number(n))) =>
        val (cpsPrv, cps) = normalizePowers(c, ps)
        if (n.compareTo(1) == 0) {
          (useDirectly(normalizePowersRV, Seq(("c_", c.lhs), ("ps_", ps), ("cps_", cps), ("v_", v)), Seq(cpsPrv)), Times(cps, v))
        } else {
          (useDirectly(normalizePowersRP, Seq(("c_", c.lhs), ("ps_", ps), ("cps_", cps), ("t_", t)), Seq(cpsPrv)), Times(cps, t))
        }
      case Number(n) =>
        //assert((n.compareTo(1) == 0))
        val (cdPrv, d) = c.normalized
        (useDirectly(normalizePowersC1, Seq(("c_", c.lhs), ("d_", d)), Seq(cdPrv)), d)
      case Power(v, Number(n)) =>
        val (cdPrv, d) = c.normalized
        if (c.num.compareTo(1) == 0 && c.denum.compareTo(1) == 0) {
          // c = 1
          if (n.compareTo(1) == 0) {
            (useDirectly(normalizePowers1V, Seq(("c_", c.lhs), ("v_", v)), Seq(cdPrv)), v)
          } else {
            (useDirectly(normalizePowers1R, Seq(("c_", c.lhs), ("t_", t)), Seq(cdPrv)), t)
          }
        } else {
          // c = d
          if (n.compareTo(1) == 0) {
            (useDirectly(normalizePowersCV, Seq(("c_", c.lhs), ("d_", d), ("v_", v)), Seq(cdPrv)), Times(d, v))
          } else {
            (useDirectly(normalizePowersCP, Seq(("c_", c.lhs), ("d_", d), ("t_", t)), Seq(cdPrv)), Times(d, t))
          }
        }
    }

    /**
      * normalized: normalize coefficient, rewrite product of rhs with
      *   1 * x = x
      *   x * 1 = 1
      *   x ^ 1 = x
      * */
    def normalized : ProvableSig = {
      val (cnPrv, cn) = coeff.forgetPrv.normalized
      if (coeff.num.compareTo(0) == 0)
        useDirectly(normalizeMonom0, Seq(
          ("x_", lhs),
          ("c_", coeff.rhs),
          ("ps_", powersTerm)), Seq(prv, cnPrv))
      else if (coeff.num.compareTo(0) > 0) {
        val (cpsPrv, cps) = normalizePowers(coeff.forgetPrv, powersTerm)
        useDirectly(normalizeMonomCS, Seq(
          ("x_", lhs),
          ("c_", coeff.rhs),
          ("ps_", powersTerm),
          ("cps_", cps)
        ), Seq(prv, cpsPrv))
      } else {
        val m = -coeff.forgetPrv
        val (cpsPrv, cps) = normalizePowers(m.forgetPrv, powersTerm)
        useDirectly(normalizeMonomNCS, Seq(
          ("x_", lhs),
          ("c_", coeff.rhs),
          ("m_", m.rhs),
          ("ps_", powersTerm),
          ("cps_", cps)
        ), Seq(prv, m.prv, cpsPrv))
      }
    }

    def approx(prec: Int) : (ProvableSig, Monomial, Monomial) = {
      val (cPrv, c1, c2) = coeff.forgetPrv.approx(prec)
      (useDirectly(splitMonomial, Seq(("c_", coeff.rhs), ("x_", powersTerm), ("c1_", c1.rhs), ("c2_", c2.rhs), ("m_", lhs)),
        Seq(cPrv, prv)), Monomial(c1, powers), Monomial(c2, powers))
    }
  }

  val zez = rememberAny("0 = 0".asFormula, byUS(DerivedAxioms.equalReflex))

  val emptySprout = rememberAny("s_() = 0 & t_() = u_() -> s_() + t_() = 0 + u_() + 0".asFormula, QE & done)

  // Lemmas for insert (i.e., add monomial)

  // @todo: should these be constructed more systematically?! e.g., define common subformulas only once. would make the code more robust...
  val branch2Left  = rememberAny("t_() = l_() + v_() + r_() & l_() + x_() = lx_() -> t_() + x_() = lx_() + v_()  + r_() ".asFormula, QE & done)
  val branch2Value = rememberAny("t_() = l_() + v_() + r_() & v_() + x_() = vx_() -> t_() + x_() = l_()  + vx_() + r_() ".asFormula, QE & done)
  val branch2Right = rememberAny("t_() = l_() + v_() + r_() & r_() + x_() = rx_() -> t_() + x_() = l_()  + v_()  + rx_()".asFormula, QE & done)

  /** @note for the Left case, could actually just use [[branch2Left]] */
  val branch2GrowLeft =  rememberAny("t_() = l_() + v_() + r_() & l_() + x_() = l1_() + lv_() + l2_() -> t_() + x_() = l1_() + lv_() + l2_() + v_() + r_()                 ".asFormula, QE & done)
  val branch2GrowRight = rememberAny("t_() = l_() + v_() + r_() & r_() + x_() = r1_() + rv_() + r2_() -> t_() + x_() = l_()                  + v_() + r1_() + rv_() + r2_()".asFormula, QE & done)

  val branch3Left =   rememberAny("t_() = l_() + v_() + m_() + w_() + r_() & l_() + x_() = lx_() -> t_() + x_() = lx_() + v_()  + m_()  + w_()  + r_() ".asFormula, QE & done)
  val branch3Value1 = rememberAny("t_() = l_() + v_() + m_() + w_() + r_() & v_() + x_() = vx_() -> t_() + x_() = l_()  + vx_() + m_()  + w_()  + r_() ".asFormula, QE & done)
  val branch3Mid =    rememberAny("t_() = l_() + v_() + m_() + w_() + r_() & m_() + x_() = mx_() -> t_() + x_() = l_()  + v_()  + mx_() + w_()  + r_() ".asFormula, QE & done)
  val branch3Value2 = rememberAny("t_() = l_() + v_() + m_() + w_() + r_() & w_() + x_() = wx_() -> t_() + x_() = l_()  + v_()  + m_()  + wx_() + r_() ".asFormula, QE & done)
  val branch3Right =  rememberAny("t_() = l_() + v_() + m_() + w_() + r_() & r_() + x_() = rx_() -> t_() + x_() = l_()  + v_()  + m_()  + w_()  + rx_()".asFormula, QE & done)

  val branch3GrowLeft = rememberAny(("t_() = l_() + v_() + m_() + w_() + r_() & l_() + x_() = l1_() + lv_() + l2_() ->" +
    "t_() + x_() = (l1_() + lv_() + l2_()) + v_()  + (m_()  + w_()  + r_())").asFormula, QE & done)

  val branch3GrowMid = rememberAny(("t_() = l_() + v_() + m_() + w_() + r_() & m_() + x_() = m1_() + mv_() + m2_() ->" +
    "t_() + x_() = (l_() + v_() + m1_()) + mv_()  + (m2_()  + w_()  + r_())").asFormula, QE & done)
  val branch3GrowRight = rememberAny(("t_() = l_() + v_() + m_() + w_() + r_() & r_() + x_() = r1_() + rv_() + r2_() ->" +
    "t_() + x_() = (l_() + v_() + m_()) + w_()  + (r1_()  + rv_()  + r2_())").asFormula, QE & done)

  // Lemmas for Add
  val plusEmpty = rememberAny(("t_() = s_() & u_() = 0 -> t_() + u_() = s_()").asFormula, QE & done)
  val plusBranch2 = rememberAny(("(s_() = l_() + v_() + r_() & t_() + l_() + v_() + r_() = sum_()) ->" +
    "t_() + s_() = sum_()").asFormula, QE & done)
  val plusBranch3 = rememberAny(("(s_() = l_() + v1_() + m_() + v2_() + r_() & t_() + l_() + v1_() + m_() + v2_() + r_() = sum_()) ->" +
    "t_() + s_() = sum_()").asFormula, QE & done)

  // Lemmas for Minus
  val minusEmpty = rememberAny(("t_() = s_() & u_() = 0 -> t_() - u_() = s_()").asFormula, QE & done)
  val minusBranch2 = rememberAny(("(s_() = l_() + v_() + r_() & t_() - l_() - v_() - r_() = sum_()) ->" +
    "t_() - s_() = sum_()").asFormula, QE & done)
  val minusBranch3 = rememberAny(("(s_() = l_() + v1_() + m_() + v2_() + r_() & t_() - l_() - v1_() - m_() - v2_() - r_() = sum_()) ->" +
    "t_() - s_() = sum_()").asFormula, QE & done)

  // Lemmas for Minus Monomial
  val plusMinus = rememberAny("t_() + (-x_()) = s_() -> t_() - x_() = s_()".asFormula, QE & done)

  // Lemmas for Times Monomial
  val monTimesZero = rememberAny("t_() = 0 -> t_() * x_() = 0".asFormula, QE & done)
  val monTimesBranch2 = rememberAny(
    ("(t_() = l_() + v_() + r_() &" +
      "l_() * x_() = lx_() &" +
      "v_() * x_() = vx_() &" +
      "r_() * x_() = rx_()) -> t_() * x_() = lx_() + vx_() + rx_()").asFormula, QE & done)
  val monTimesBranch3 = rememberAny(
    ("(t_() = l_() + v1_() + m_() + v2_() + r_() &" +
      "l_() * x_() = lx_() &" +
      "v1_() * x_() = v1x_() &" +
      "m_() * x_() = mx_() &" +
      "v2_() * x_() = v2x_() &" +
      "r_() * x_() = rx_()) -> t_() * x_() = lx_() + v1x_() + mx_() + v2x_() + rx_()").asFormula, QE & done)

  // Lemmas for Times
  val timesEmpty = rememberAny(("t_() = 0 -> t_() * u_() = 0").asFormula, QE & done)
  val timesBranch2 = rememberAny(("(t_() = l_() + v_() + r_() & l_()*u_() + u_() * v_() + r_()*u_() = sum_()) ->" +
    "t_() * u_() = sum_()").asFormula, QE & done)
  val timesBranch3 = rememberAny(("(t_() = l_() + v1_() + m_() + v2_() + r_() & l_()*u_() + u_()*v1_() + m_()*u_() + u_()*v2_() + r_()*u_() = sum_()) ->" +
    "t_() * u_() = sum_()").asFormula, QE & done)

  // Lemmas for Power
  lazy val powerZero = rememberAny(("1 = one_() -> t_() ^ 0 = one_()").asFormula, QE & done)
  lazy val powerOne = rememberAny(("t_() = s_() -> t_() ^ 1 = s_()").asFormula, QE & done)
  val powerEven = rememberAny(("((n_() = 2*m_() <-> true) & t_()^m_() = p_() & p_()*p_() = r_()) ->" +
    "t_() ^ n_() = r_()").asFormula,
    implyR(1) & andL(-1) & andL(-2) &
      useAt(DerivedAxioms.equivTrue, PosInExpr(0::Nil))(-1) &
      eqL2R(-1)(1) & hideL(-1) &
      cutR("t_() ^ (2*m_()) = (t_()^m_())^2".asFormula)(1) & Idioms.<(
      QE & done,
      implyR(1) & eqL2R(-3)(1) & hideL(-3) & eqL2R(-1)(1) & hideL(-1) & QE & done
    )
  )
  val powerOdd = rememberAny(("((n_() = 2*m_() + 1 <-> true) & t_()^m_() = p_() & p_()*p_()*t_() = r_()) ->" +
    "t_() ^ n_() = r_()").asFormula,
    implyR(1) & andL(-1) & andL(-2) &
      useAt(DerivedAxioms.equivTrue, PosInExpr(0::Nil))(-1) &
      eqL2R(-1)(1) & hideL(-1) &
      cutR("t_() ^ (2*m_() + 1) = (t_()^m_())^2*t_()".asFormula)(1) & Idioms.<(
      QE & done,
      implyR(1) & eqL2R(-3)(1) & hideL(-3) & eqL2R(-1)(1) & hideL(-1) & QE & done
    )
  )

  // Lemmas for negation
  val negateEmpty = rememberAny("t_() = 0 -> -t_() = 0".asFormula, QE & done)
  val negateBranch2 = rememberAny(("(t_() = l_() + v_() + r_() & -l_() = nl_() & -v_() = nv_() & -r_() = nr_()) ->" +
    "-t_() = nl_() + nv_() + nr_()").asFormula, QE & done)
  val negateBranch3 = rememberAny(("(t_() = l_() + v1_() + m_() + v2_() + r_() & -l_() = nl_() & -v1_() = nv1_() & -m_() = nm_() & -v2_() = nv2_() & -r_() = nr_()) ->" +
    "-t_() = nl_() + nv1_() + nm_() + nv2_() + nr_()").asFormula, QE & done)


  // Lemmas for normalization
  val normalizeCoeff0 = rememberAny("(c_() = 0 / d_() ) -> c_() = 0".asFormula, QE & done)
  val normalizeCoeff1 = rememberAny("(c_() = n_() / 1 ) -> c_() = n_()".asFormula, QE & done)

  val normalizeMonom0 = rememberAny("(x_() = c_() * ps_() & c_() = 0) -> x_() = 0".asFormula, QE & done)
  val normalizeMonomCS = rememberAny(("(x_() = c_() * ps_() & c_() * ps_() = cps_()) ->" +
    "x_() = cps_()").asFormula, QE & done)
  val normalizeMonomNCS = rememberAny(("(x_() = c_() * ps_() & -c_() = m_() & m_() * ps_() = cps_()) ->" +
    "x_() = -cps_()").asFormula, QE & done)

  val normalizePowers1V = rememberAny("(c_() = 1) -> c_() * v_()^1 = v_()".asFormula, QE & done)
  val normalizePowers1R = rememberAny("(c_() = 1) -> c_() * t_() = t_()".asFormula, QE & done)
  val normalizePowersC1 = rememberAny("(c_() = d_()) -> c_() * 1 = d_()".asFormula, QE & done)
  val normalizePowersCV = rememberAny("(c_() = d_()) -> c_() * v_()^1 = d_()*v_()".asFormula, QE & done)
  val normalizePowersCP = rememberAny("(c_() = d_()) -> c_() * t_() = d_()*t_()".asFormula, QE & done)
  val normalizePowersR1 = rememberAny("(c_() * ps_() = cps_()) -> c_() * (ps_() * 1) = cps_()".asFormula, QE & done)
  val normalizePowersRV = rememberAny("(c_() * ps_() = cps_()) -> c_() * (ps_() * v_()^1) = cps_() * v_()".asFormula, QE & done)
  val normalizePowersRP = rememberAny("(c_() * ps_() = cps_()) -> c_() * (ps_() * t_()) = cps_() * t_()".asFormula, QE & done)

  val normalizeBranch2 = rememberAny(("(t_() = l_() + v_() + r_() & l_() = ln_() & v_() = vn_() & r_() = rn_()) ->" +
    "t_() = ln_() + vn_() + rn_()").asFormula, QE & done)
  val normalizeBranch3 = rememberAny(("(t_() = l_() + v1_() + m_() + v2_() + r_() & l_() = ln_() & v1_() = v1n_() & m_() = mn_() & v2_() = v2n_() & r_() = rn_()) ->" +
    "t_() = ln_() + v1n_() + mn_() + v2n_() + rn_()").asFormula, QE & done)

  val reassocRight0 = rememberAny((
    "(" +
      "t_() = l_() + r_() &" +
      "r_() = 0   &" +
      "l_() = ll_()" +
      ") ->" +
      "t_() = ll_()").asFormula, QE & done)
  val reassocRightPlus = rememberAny((
    "(" +
      "t_() = l_() + r_() &" +
      "r_() = rs_() + rr_() &" +
      "l_() + rs_() = lrs_()" +
      ") ->" +
      "t_() = lrs_() + rr_()").asFormula, QE & done)
  val reassocLeft0RightConst = rememberAny((
    "(" +
      "t_() = l_() + r_() &" +
      "r_() = c_() &" +
      "l_() = 0" +
      ") ->" +
      "t_() = c_()").asFormula, QE & done)
  val reassocRightConst = rememberAny((
    "(" +
      "t_() = l_() + r_() &" +
      "r_() = c_() &" +
      "l_() = ll_()" +
      ") ->" +
      "t_() = ll_() + c_()").asFormula, QE & done)

  // Lemmas for partition
  val partition2 = rememberAny(("(t_() = r_() & t1_() = r1_() & t2_() = r2_() & t_() - t1_() - t2_() = 0) -> t_() = t1_() + t2_()".asFormula),
    QE & done)

  // Lemmas for splitting coefficients
  @inline
  private def nz(t: Term) : Formula = NotEqual(t, Number(0))
  // @todo: compute ``instantiations'' like this everywhere and prove by matching?
  def splitCoefficientNumericCondition(n: Term, d: Term, n1: Term, d1: Term, n2: Term, d2: Term) =
    And(Equal(Times(Times(n, d1), d2), Times(d, Plus(Times(d1, n2), Times(d2, n1)))), And(nz(d), And(nz(d1), nz(d2))))

  val splitCoefficient = rememberAny(
    Imply(And("c_() = n_()/d_()".asFormula,
      Equiv(splitCoefficientNumericCondition("n_()".asTerm, "d_()".asTerm, "n1_()".asTerm, "d1_()".asTerm, "n2_()".asTerm, "d2_()".asTerm), True)),
      "c_() = n1_()/d1_() + n2_()/d2_()".asFormula),
    QE & done)
  val splitMonomial = rememberAny("(c_() = c1_() + c2_() & m_() = c_() * x_()) -> m_() = c1_() * x_() + c2_() * x_()".asFormula, QE & done)
  val splitEmpty  = rememberAny("t_() = 0 -> t_() = 0 + 0".asFormula, QE & done)
  val splitBranch2  = rememberAny(("(t_() = l_() + v_() + r_() & l_() = l1_() + l2_() & v_() = v1_() + v2_() & r_() = r1_() + r2_())" +
    "->" +
    "t_() = (l1_() + v1_() + r1_()) + (l2_() + v2_() + r2_())").asFormula, QE & done)
  val splitBranch3  = rememberAny(("(t_() = l_() + v1_() + m_() + v2_() + r_() & l_() = l1_() + l2_() & v1_() = v11_() + v12_() & m_() = m1_() + m2_() & v2_() = v21_() + v22_() & r_() = r1_() + r2_())" +
    "->" +
    "t_() = (l1_() + v11_() + m1_() + v21_() + r1_()) + (l2_() + v12_() + m2_() + v22_() + r2_())").asFormula, QE & done)


  /** drop parentheses of a sum of terms on the rhs of prv to the left, e.g.,
    * t = a + (b + c) ~~> t = a + b + c
    * */
  def reassoc(prv: ProvableSig) : ProvableSig = rhsOf(prv) match {
    case Plus(l, r) =>
      val rPrv = reassoc(equalReflex(r))
      rhsOf(rPrv) match {
        case Number(n) if n.compareTo(0) == 0 =>
          val llPrv = reassoc(equalReflex(l))
          useDirectly(reassocRight0, Seq(
            ("t_", lhsOf(prv)),
            ("l_", l),
            ("r_", r),
            ("ll_", rhsOf(llPrv))
          ), Seq(prv, rPrv, llPrv))
        case Plus(rs, rr) =>
          val lrsPrv = reassoc(equalReflex(Plus(l, rs)))
          useDirectly (reassocRightPlus, Seq(
            ("t_", lhsOf(prv)),
            ("l_", l),
            ("r_", r),
            ("rs_", rs),
            ("rr_", rr),
            ("lrs_", rhsOf(lrsPrv))
          ), Seq(prv, rPrv, lrsPrv))
        case c =>
          val llPrv = reassoc(equalReflex(l))
          rhsOf(llPrv) match {
            case Number(n) if n.compareTo(0) == 0 =>
              useDirectly (reassocLeft0RightConst, Seq(
                ("t_", lhsOf(prv)),
                ("l_", l),
                ("r_", r),
                ("c_", c)
              ), Seq(prv, rPrv, llPrv))
            case ll =>
              useDirectly (reassocRightConst, Seq(
                ("t_", lhsOf(prv)),
                ("l_", l),
                ("r_", r),
                ("c_", c),
                ("ll_", ll)
              ), Seq(prv, rPrv, llPrv))
          }
      }
    case _ =>
      prv
  }

  /**
    * 2-3 Tree for monomials, keeping track of proofs.
    * */
  sealed trait Growth
  case class Stay(p: TreePolynomial) extends Growth
  case class Sprout(sprout: Branch2) extends Growth

  sealed trait TreePolynomial extends Polynomial {
    val prv: ProvableSig
    def representation: ProvableSig = prv
    def forgetPrv: TreePolynomial
    def resetTerm: Polynomial = forgetPrv

    def treeSketch: String
    lazy val (eq, lhs, rhs) = prv.conclusion.succ(0) match { case eq @ Equal(lhs, rhs) => (eq, lhs, rhs) }
    lazy val term = lhs

    def lookup(x: Monomial) : Option[Monomial] = this match {
      case Empty(_) => None
      case Branch2(left, v, right, _) => x.compare(v) match {
        case 0 => Some(v)
        case c if c < 0 => left.lookup(x)
        case c if c > 0 => right.lookup(x)
      }
      case Branch3(left, v1, mid, v2, right, _) => x.compare(v1) match {
        case 0 => Some(v1)
        case c if c < 0 => left.lookup(x)
        case c if c > 0 => x.compare(v2) match {
          case 0 => Some(v2)
          case c if c < 0 => mid.lookup(x)
          case c if c > 0 => right.lookup(x)
        }
      }
    }


    // addition

    private def insert(x: Monomial) : Growth = this match {
      case Empty(_) =>
        val newPrv = useDirectly(emptySprout, Seq(("s_", lhs),("t_", x.lhs),("u_", x.rhs)), Seq(prv, x.prv))
        Sprout(Branch2(Empty(None), x, Empty(None), Some(newPrv)))
      case tree @ Branch2(left, v, right, prv) =>
        val newLhs = Plus(tree.lhs, x.lhs)
        val treeInst = IndexedSeq(
          ("t_", tree.lhs),
          ("v_", v.rhs),
          ("x_", x.lhs),
          ("l_", left.rhs),
          ("r_", right.rhs)
        )
        x.compare(v) match {
        case 0 =>
          val vx = (v.forgetPrv+x).get
          val newRhs = Plus(Plus(left.rhs, vx.rhs), right.rhs)
          val newPrv = useDirectly(branch2Value, treeInst ++ Seq(("vx_", vx.rhs)), Seq(tree.prv, vx.prv))
          Stay(Branch2(left, vx, right, Some(newPrv)))
        case c if c < 0 => {
          left.forgetPrv.insert(x) match {
            case Sprout(lx) =>
              val l1 = lx.left.rhs
              val lv = lx.value.rhs
              val l2 = lx.right.rhs
              val newRhs = Seq(l1, lv, l2, v.rhs, right.rhs).reduceLeft(Plus)
              val newPrv = useDirectly(branch2GrowLeft, treeInst ++ Seq(("l1_", l1), ("lv_", lv) , ("l2_", l2)), Seq(tree.prv, lx.prv))
              Stay(Branch3(lx.left, lx.value, lx.right, v, right, Some(newPrv)))
            case Stay(lx) =>
              val newRhs = Plus(Plus(lx.rhs, v.rhs), right.rhs)
              val newPrv = useDirectly(branch2Left, treeInst ++ Seq(("lx_", lx.rhs)), Seq(tree.prv, lx.prv))
              Stay(Branch2(lx, v, right, Some(newPrv)))
          }
        }
        case c if c > 0 =>  {
          right.forgetPrv.insert(x) match {
            case Sprout(rx) =>
              val r1 = rx.left.rhs
              val rv = rx.value.rhs
              val r2 = rx.right.rhs
              val newRhs = Seq(left.rhs, v.rhs, r1, rv, r2).reduceLeft(Plus)
              val newPrv = useDirectly(branch2GrowRight, treeInst ++ Seq(("r1_", r1),("rv_", rv),("r2_", r2)), Seq(tree.prv, rx.prv))
              Stay(Branch3(left, v, rx.left, rx.value, rx.right, Some(newPrv)))
            case Stay(rx) =>
              val newRhs = Plus(Plus(left.rhs, v.rhs), rx.rhs)
              val newPrv = useDirectly(branch2Right, treeInst ++ Seq(("rx_", rx.rhs)), Seq(tree.prv, rx.prv))
              Stay(Branch2(left, v, rx, Some(newPrv)))
          }
        }
      }
      case tree @ Branch3(left, v, mid, w, right, prv) =>
        val newLhs = Plus(tree.lhs, x.lhs)
        val treeInst = IndexedSeq(
          ("t_", tree.lhs),
          ("x_", x.lhs),
          ("l_", left.rhs),
          ("v_", v.rhs),
          ("m_", mid.rhs),
          ("w_", w.rhs),
          ("r_", right.rhs)
        )
        x.compare(v) match {
          case 0 =>
            val vx = (v.forgetPrv + x).get
            val newRhs = Seq(left.rhs, vx.rhs, mid.rhs, w.rhs, right.rhs).reduceLeft(Plus)
            val newPrv = useDirectly(branch3Value1, treeInst ++ Seq(("vx_", vx.rhs)), Seq(tree.prv, vx.prv))
            Stay(Branch3(left, vx, mid, w, right, Some(newPrv)))
          case c if c < 0 => left.forgetPrv.insert(x) match {
            case Stay(lx) =>
              val newRhs = Seq(lx.rhs, v.rhs, mid.rhs, w.rhs, right.rhs).reduceLeft(Plus)
              val newPrv = useDirectly(branch3Left, treeInst ++ Seq(("lx_", lx.rhs)), Seq(tree.prv, lx.prv))
              Stay(Branch3(lx, v, mid, w, right, Some(newPrv)))
            case Sprout(lx) =>
              val l1 = lx.left.rhs
              val lv = lx.value.rhs
              val l2 = lx.right.rhs
              val newRhs = Seq(Seq(l1, lv, l2).reduceLeft(Plus), v.rhs, Seq(mid.rhs, w.rhs, right.rhs).reduceLeft(Plus)).reduceLeft(Plus)
              val newPrv = useDirectly(branch3GrowLeft, treeInst ++ Seq(("l1_", l1), ("lv_", lv), ("l2_", l2)), Seq(tree.prv, lx.prv))
              Sprout(Branch2(lx, v, Branch2(mid, w, right, None), Some(newPrv)))
          }
          case c if c > 0 =>
            x.compare(w) match {
              case 0 =>
                val wx = (w.forgetPrv + x).get
                val newRhs = Seq(left.rhs, v.rhs, mid.rhs, wx.rhs, right.rhs).reduceLeft(Plus)
                val newPrv = useDirectly(branch3Value2, treeInst ++ Seq(("wx_", wx.rhs)), Seq(tree.prv, wx.prv))
                Stay(Branch3(left, v, mid, wx, right, Some(newPrv)))
              case c if c < 0 =>
                mid.forgetPrv.insert(x) match {
                  case Stay(mx) =>
                    val newRhs = Seq(left.rhs, v.rhs, mx.rhs, w.rhs, right.rhs).reduceLeft(Plus)
                    val newPrv = useDirectly(branch3Mid, treeInst ++ Seq(("mx_", mx.rhs)), Seq(tree.prv, mx.prv))
                    Stay(Branch3(left, v, mx, w, right, Some(newPrv)))
                  case Sprout(mx) =>
                    val m1 = mx.left.rhs
                    val mv = mx.value.rhs
                    val m2 = mx.right.rhs
                    val newRhs = Seq(Seq(left.rhs, v.rhs, m1).reduceLeft(Plus), mv, Seq(m2, w.rhs, right.rhs).reduceLeft(Plus)).reduceLeft(Plus)
                    val newPrv = useDirectly(branch3GrowMid, treeInst ++ Seq(("m1_", m1), ("mv_", mv), ("m2_", m2)), Seq(tree.prv, mx.prv))
                    Sprout(Branch2(Branch2(left, v, mx.left, None), mx.value, Branch2(mx.right, w, right, None), Some(newPrv)))
                }
              case c if c > 0 =>
                right.forgetPrv.insert(x) match {
                  case Stay(rx) =>
                    val newRhs = Seq(left.rhs, v.rhs, mid.rhs, w.rhs, rx.rhs).reduceLeft(Plus)
                    val newPrv = useDirectly(branch3Right, treeInst ++ Seq(("rx_", rx.rhs)), Seq(tree.prv, rx.prv))
                    Stay(Branch3(left, v, mid, w, rx, Some(newPrv)))
                  case Sprout(rx) =>
                    val r1 = rx.left.rhs
                    val rv = rx.value.rhs
                    val r2 = rx.right.rhs
                    val newRhs = Seq(Seq(left.rhs, v.rhs, mid.rhs).reduceLeft(Plus), w.rhs, Seq(r1, rv, r2).reduceLeft(Plus)).reduceLeft(Plus)
                    val newPrv = useDirectly(branch3GrowRight, treeInst ++ Seq(("r1_", r1), ("rv_", rv), ("r2_", r2)), Seq(tree.prv, rx.prv))
                    Sprout(Branch2(Branch2(left, v, mid, None), w, rx, Some(newPrv)))
                }
            }
        }
    }
    def +(m: Monomial) : TreePolynomial = insert(m) match {
      case Stay(p) => p
      case Sprout(s) => s
    }

    def -(m: Monomial) : TreePolynomial = {
      val res = this + (-(m.forgetPrv))
      res.updatePrv(useDirectly(plusMinus, Seq(("t_", lhs), ("x_", m.lhs), ("s_", res.rhs)), Seq(res.prv)))
    }

    private[TwoThreeTreePolynomialRing] def updatePrv(prv2: ProvableSig) : TreePolynomial = {
      this match {
        case Empty(_) => Empty(Some(prv2))
        case Branch2(l, v, m, _) => Branch2(l, v, m, Some(prv2))
        case Branch3(l, v1, m, v2, r, _) => Branch3(l, v1, m, v2, r, Some(prv2))
      }
    }

    def +(other: Polynomial) : TreePolynomial = other match {
      case other @ Empty(_) =>
        val newPrv = useDirectly(plusEmpty, Seq(("t_", lhs), ("s_", rhs), ("u_", other.lhs)), Seq(prv, other.prv))
        updatePrv(newPrv)
      case other @ Branch2(left, value, right, _) =>
        val sum = this + left.forgetPrv + value.forgetPrv + right.forgetPrv
        val newPrv = useDirectly(plusBranch2, IndexedSeq(
            ("t_", lhs),
            ("s_", other.lhs),
            ("l_", left.rhs),
            ("v_", value.rhs),
            ("r_", right.rhs),
            ("sum_", sum.rhs)
          ), Seq(other.prv, sum.prv))
        sum.updatePrv(newPrv)
      case other @ Branch3(left, value1, mid, value2, right, _) =>
        val sum = this + left.forgetPrv + value1.forgetPrv + mid.forgetPrv + value2.forgetPrv + right.forgetPrv
        val newPrv = useDirectly(plusBranch3, IndexedSeq(
            ("t_", lhs),
            ("s_", other.lhs),
            ("l_", left.rhs),
            ("v1_", value1.rhs),
            ("m_", mid.rhs),
            ("v2_", value2.rhs),
            ("r_", right.rhs),
            ("sum_", sum.rhs)
          ), Seq(other.prv, sum.prv))
        sum.updatePrv(newPrv)
      case _ => throw new RuntimeException("only TreePolynomials are supported, but got " + other)
    }

    def -(other: Polynomial) : TreePolynomial = other match {
      case other @ Empty(_) =>
        val newPrv = useDirectly(minusEmpty, Seq(("t_", lhs), ("s_", rhs), ("u_", other.lhs)), Seq(prv, other.prv))
        updatePrv(newPrv)
      case other @ Branch2(left, value, right, _) =>
        val sum = this - left.forgetPrv - value.forgetPrv - right.forgetPrv
        val newPrv = useDirectly(minusBranch2, IndexedSeq(
          ("t_", lhs),
          ("s_", other.lhs),
          ("l_", left.rhs),
          ("v_", value.rhs),
          ("r_", right.rhs),
          ("sum_", sum.rhs)
        ), Seq(other.prv, sum.prv))
        sum.updatePrv(newPrv)
      case other @ Branch3(left, value1, mid, value2, right, _) =>
        val sum = this - left.forgetPrv - value1.forgetPrv - mid.forgetPrv - value2.forgetPrv - right.forgetPrv
        val newPrv = useDirectly(minusBranch3, IndexedSeq(
          ("t_", lhs),
          ("s_", other.lhs),
          ("l_", left.rhs),
          ("v1_", value1.rhs),
          ("m_", mid.rhs),
          ("v2_", value2.rhs),
          ("r_", right.rhs),
          ("sum_", sum.rhs)
        ), Seq(other.prv, sum.prv))
        sum.updatePrv(newPrv)
      case _ => throw new RuntimeException("only TreePolynomials are supported, but got " + other)
    }

    def *(x: Monomial) : TreePolynomial = this match {
      case Empty(_) =>
        val newPrv = useDirectly(monTimesZero, Seq(("t_", lhs), ("x_", x.lhs)), Seq(prv))
        Empty(Some(newPrv))
      case Branch2(l, v, r, _) =>
        val lx = l.forgetPrv * x
        val vx = v.forgetPrv * x
        val rx = r.forgetPrv * x
        val newPrv = useDirectly(monTimesBranch2, IndexedSeq(
            ("t_", lhs),
            ("x_", x.lhs),
            ("l_", l.rhs),
            ("v_", v.rhs),
            ("r_", r.rhs),
            ("lx_", lx.rhs),
            ("vx_", vx.rhs),
            ("rx_", rx.rhs)), Seq(prv, lx.prv, vx.prv, rx.prv))
        Branch2(lx, vx, rx, Some(newPrv))
      case Branch3(l, v1, m, v2, r, _) =>
        val lx = l.forgetPrv * x
        val v1x = v1.forgetPrv * x
        val mx = m.forgetPrv * x
        val v2x = v2.forgetPrv * x
        val rx = r.forgetPrv * x
        val newPrv = useDirectly(monTimesBranch3, IndexedSeq(
            ("t_", lhs),
            ("x_", x.lhs),
            ("l_", l.rhs),
            ("v1_", v1.rhs),
            ("m_", m.rhs),
            ("v2_", v2.rhs),
            ("r_", r.rhs),
            ("lx_", lx.rhs),
            ("v1x_", v1x.rhs),
            ("mx_", mx.rhs),
            ("v2x_", v2x.rhs),
            ("rx_", rx.rhs)
          ), Seq(prv, lx.prv, v1x.prv, mx.prv, v2x.prv, rx.prv))
        Branch3(lx, v1x, mx, v2x, rx, Some(newPrv))
    }

    def *(other: Polynomial): TreePolynomial = other match {
      case other: TreePolynomial =>
        this match {
          case Empty(_) =>
            val newPrv = useDirectly(timesEmpty, Seq(("t_", lhs), ("u_", other.lhs)), Seq(prv))
            updatePrv(newPrv)
          case Branch2(left, value, right, _) =>
            val sum = (left.forgetPrv * other) + (other * value.forgetPrv) + (right.forgetPrv * other)
            val newPrv = useDirectly(timesBranch2, IndexedSeq(
              ("t_", lhs),
              ("u_", other.lhs),
              ("l_", left.rhs),
              ("v_", value.rhs),
              ("r_", right.rhs),
              ("sum_", sum.rhs)
            ), Seq(prv, sum.prv))
            sum.updatePrv(newPrv)
          case Branch3(left, value1, mid, value2, right, _) =>
            val sum = (left.forgetPrv * other) + (other * value1.forgetPrv) + (mid.forgetPrv * other) + (other * value2.forgetPrv) + (right.forgetPrv * other)
            val newPrv = useDirectly(timesBranch3, IndexedSeq(
              ("t_", lhs),
              ("u_", other.lhs),
              ("l_", left.rhs),
              ("v1_", value1.rhs),
              ("m_", mid.rhs),
              ("v2_", value2.rhs),
              ("r_", right.rhs),
              ("sum_", sum.rhs)
            ), Seq(prv, sum.prv))
            sum.updatePrv(newPrv)
        }
      case _ => throw new RuntimeException("only TreePolynomials are supported, but got " + other)
    }

    def ^(n: Int) : TreePolynomial = n match {
      case 0 =>
        One.updatePrv(useDirectly(powerZero, IndexedSeq(("t_", lhs), ("one_", One.rhs)), Seq(One.prv)))
      case 1 =>
        this.updatePrv(useDirectly(powerOne, IndexedSeq(("t_", lhs), ("s_", rhs)), Seq(prv)))
      case n =>
        if (n >= 0) {
          if (n % 2 == 0) {
            val m = n / 2
            val mPrv = ProvableSig.proveArithmetic(BigDecimalQETool, Equal(Number(n), Times(Number(2), Number(m))))
            val p = this^(m)
            val r = p.forgetPrv*p.forgetPrv
            val newPrv = useDirectly(powerEven,
              Seq(("n_", Number(n)), ("m_", Number(m)), ("t_", lhs), ("p_", p.rhs), ("r_", r.rhs)),
              Seq(mPrv, p.prv, r.prv))
            r.updatePrv(newPrv)
          } else {
            val m = n / 2
            val mPrv = ProvableSig.proveArithmetic(BigDecimalQETool, Equal(Number(n), Plus(Times(Number(2), Number(m)), Number(1))))
            val p = this^(m)
            val r = p.forgetPrv*p.forgetPrv*this
            val newPrv = useDirectly(powerOdd,
              Seq(("n_", Number(n)), ("m_", Number(m)), ("t_", lhs), ("p_", p.rhs), ("r_", r.rhs)),
              Seq(mPrv, p.prv, r.prv))
            r.updatePrv(newPrv)
          }
        } else throw new IllegalArgumentException("negative power unsupported by PolynomialArithV2")
    }

    // negation
    def unary_- : TreePolynomial = this match {
      case Empty(_) => Empty(Some(useDirectly(negateEmpty, Seq(("t_", lhs)), Seq(prv))))
      case Branch2(l, v, r, _) =>
        val nl = -(l.forgetPrv)
        val nv = -(v.forgetPrv)
        val nr = -(r.forgetPrv)
        val newPrv = useDirectly(negateBranch2, Seq(
          ("t_", lhs),
          ("l_", l.rhs),
          ("v_", v.rhs),
          ("r_", r.rhs),
          ("nl_", nl.rhs),
          ("nv_", nv.rhs),
          ("nr_", nr.rhs),
        ), Seq(prv, nl.prv, nv.prv, nr.prv))
        Branch2(nl, nv, nr, Some(newPrv))
      case Branch3(l, v1, m, v2, r, _) =>
        val nl = -(l.forgetPrv)
        val nv1 = -(v1.forgetPrv)
        val nm = -(m.forgetPrv)
        val nv2 = -(v2.forgetPrv)
        val nr = -(r.forgetPrv)
        val newPrv = useDirectly(negateBranch3, Seq(
          ("t_", lhs),
          ("l_", l.rhs),
          ("v1_", v1.rhs),
          ("m_", m.rhs),
          ("v2_", v2.rhs),
          ("r_", r.rhs),
          ("nl_", nl.rhs),
          ("nv1_", nv1.rhs),
          ("nm_", nm.rhs),
          ("nv2_", nv2.rhs),
          ("nr_", nr.rhs),
        ), Seq(prv, nl.prv, nv1.prv, nm.prv, nv2.prv, nr.prv))
        Branch3(nl, nv1, nm, nv2, nr, Some(newPrv))
    }


    /** only normalize monomials, keep 0s and binary tree association */
    def normalizedMonomials: ProvableSig = this match {
      case Empty(_) => prv
      case Branch2(l, v, r, _) =>
        val lnPrv = l.forgetPrv.normalizedMonomials
        val vnPrv = v.forgetPrv.normalized
        val rnPrv = r.forgetPrv.normalizedMonomials
        useDirectly(normalizeBranch2,
          Seq(
            ("t_", lhs),
            ("l_", l.rhs), ("v_", v.rhs), ("r_", r.rhs),
            ("ln_", rhsOf(lnPrv)), ("vn_", rhsOf(vnPrv)), ("rn_", rhsOf(rnPrv))
          ),
          Seq(prv, lnPrv, vnPrv, rnPrv))
      case Branch3(l, v1, m, v2, r, _) =>
        val lnPrv = l.forgetPrv.normalizedMonomials
        val v1nPrv = v1.forgetPrv.normalized
        val mnPrv = m.forgetPrv.normalizedMonomials
        val v2nPrv = v2.forgetPrv.normalized
        val rnPrv = r.forgetPrv.normalizedMonomials
        useDirectly(normalizeBranch3,
          Seq(
            ("t_", lhs),
            ("l_", l.rhs), ("v1_", v1.rhs), ("m_", m.rhs), ("v2_", v2.rhs), ("r_", r.rhs),
            ("ln_", rhsOf(lnPrv)), ("v1n_", rhsOf(v1nPrv)), ("mn_", rhsOf(mnPrv)), ("v2n_", rhsOf(v2nPrv)), ("rn_", rhsOf(rnPrv))
          ),
          Seq(prv, lnPrv, v1nPrv, mnPrv, v2nPrv, rnPrv))
    }

    /** normalized to nicer rhs: drop 0 for empty leaves, normalized monomials, reassociated
      * e.g., t = (0 + a + 0) + b + (0 + c + 0 + d + 0) ~~> t = a + b + c + d
      * */
    def normalized: ProvableSig = reassoc(normalizedMonomials)

    def prettyRepresentation: ProvableSig = normalized

    def zeroTest: Option[ProvableSig] = {
      val normalizedPrv = normalized
      rhsOf(normalizedPrv) match {
        case Number(n) if n.compareTo(0) == 0 =>
          Some(normalizedPrv)
        case _ => None
      }
    }

    def partitionMonomials(P: Monomial => Boolean)(acc: (Seq[Monomial], Seq[Monomial])) : (Seq[Monomial], Seq[Monomial]) = {
      def accumulate(m: Monomial)(acc: (Seq[Monomial], Seq[Monomial])) :  (Seq[Monomial], Seq[Monomial]) = acc match {
        case (pos, neg) =>
          if (P(m)) (m +: pos, neg)
          else (pos, m +: neg)
      }
      this match {
        case Empty(_) => acc
        case Branch2(left, value, right, _) =>
          right.partitionMonomials(P)(accumulate(value)(left.partitionMonomials(P)(acc)))
        case Branch3(left, value1, mid, value2, right, _) =>
          right.partitionMonomials(P)(accumulate(value2)(mid.partitionMonomials(P)(accumulate(value1)(left.partitionMonomials(P)(acc)))))
      }
    }

    def ofMonomials(monomials: Seq[Monomial]): TreePolynomial = monomials.foldLeft[TreePolynomial](Empty(None))(_ + _)

    def partition(P: (BigDecimal, BigDecimal, IndexedSeq[Int]) => Boolean): (Polynomial, Polynomial, ProvableSig) = {
      def PMonomial(m: Monomial) : Boolean = P(m.coeff.num, m.coeff.denum, m.powers)
      val (pos, neg) = partitionMonomials(PMonomial)(Seq(), Seq())
      val p1 = ofMonomials(pos)
      val p2 = ofMonomials(neg)
      val prv0 = (this - p1 - p2).zeroTest.getOrElse(throw new RuntimeException("Runtime error in 0-proof for partitioning - this should never fail!"))
      val eqPrv = useDirectly(partition2,
        Seq(
          ("t_", lhs), ("r_", rhs),
          ("t1_", p1.lhs), ("r1_", p1.rhs),
          ("t2_", p2.lhs), ("r2_", p2.rhs),
        ),
        Seq(prv, p1.prv, p2.prv, prv0))
      (p1, p2, eqPrv)
    }

    def approx(prec: Int) : (ProvableSig, TreePolynomial, TreePolynomial) = this match {
      case Empty(_) =>
        (useDirectly(splitEmpty, Seq(("t_", lhs)), Seq(prv)), Empty(None), Empty(None))
      case Branch2(left, value, right, _) =>
        val (lPrv, l1, l2) = left.forgetPrv.approx(prec)
        val (rPrv, r1, r2) = right.forgetPrv.approx(prec)
        val (vPrv, v1, v2) = value.forgetPrv.approx(prec)
        (useDirectly(splitBranch2, Seq(("t_", lhs),
          ("l_", left.rhs), ("v_", value.rhs), ("r_", right.rhs),
          ("l1_", l1.rhs), ("v1_", v1.rhs), ("r1_", r1.rhs),
          ("l2_", l2.rhs), ("v2_", v2.rhs), ("r2_", r2.rhs)
        ), Seq(prv, lPrv, vPrv, rPrv)),
          Branch2(l1, v1, r1, None),
          Branch2(l2, v2, r2, None))
      case Branch3(left, value1, middle, value2, right, _) =>
        val (lPrv, l1, l2) = left.forgetPrv.approx(prec)
        val (v1Prv, v11, v12) = value1.forgetPrv.approx(prec)
        val (mPrv, m1, m2) = middle.forgetPrv.approx(prec)
        val (v2Prv, v21, v22) = value2.forgetPrv.approx(prec)
        val (rPrv, r1, r2) = right.forgetPrv.approx(prec)
        (useDirectly(splitBranch3, Seq(("t_", lhs),
          ("l_", left.rhs), ("v1_", value1.rhs), ("m_", middle.rhs), ("v2_", value2.rhs), ("r_", right.rhs),
          ("l1_", l1.rhs), ("v11_", v11.rhs), ("m1_", m1.rhs), ("v12_", v12.rhs), ("r1_", r1.rhs),
          ("l2_", l2.rhs), ("v21_", v21.rhs), ("m2_", m2.rhs), ("v22_", v22.rhs), ("r2_", r2.rhs)
        ), Seq(prv, lPrv, v1Prv, mPrv, v2Prv, rPrv)),
          Branch3(l1, v11, m1, v21, r1, None),
          Branch3(l2, v12, m2, v22, r2, None))
    }

  }

  val varPowerLemmas = (0 until variables.length).map(i => proveBy(Equal(Power(variables(i), "i_()".asTerm),
    Seq(Number(0), Times("1/1".asTerm,
      (0 until variables.length).map(j =>
        if (i == j) Power(variables(i), "i_()".asTerm) else Number(1)).reduceLeft(Times)), Number(0)).reduceLeft(Plus)), QE & done))

  // Constructors
  def Var(index: Int, power: Int) : TreePolynomial =
    if(power == 0) ???
    else
      Branch2(Empty(None), Monomial(Coefficient(1, 1),
        (0 until variables.length).map(i => if (i == index) power else 0)), Empty(None), Some(
          varPowerLemmas(index)(USubst(Seq(SubstitutionPair(constR("i_"), Number(power)))))))

  val varLemmas = (0 until variables.length).map(i => proveBy(Equal(variables(i),
    Seq(Number(0), Times("1/1".asTerm,
      (0 until variables.length).map(j =>
        if (i == j) Power(variables(i), Number(1)) else Number(1)).reduceLeft(Times)), Number(0)).reduceLeft(Plus)), QE & done))
  def Var(index: Int) : TreePolynomial =
    Branch2(Empty(None), Monomial(Coefficient(1, 1), (0 until variables.length).map(i => if (i == index) 1 else 0)), Empty(None), Some(
      varLemmas(index)))


  val constLemma = rememberAny(
    Equal("n_()".asTerm, Seq(Number(0), Times(Divide(constR("n_"), Number(1)), (0 until variables.length).map(_ => Number(1)).reduceLeft(Times)), Number(0)).reduceLeft(Plus)),
    QE & done)
  val rationalLemma = rememberAny(
    Equal("n_() / d_()".asTerm, Seq(Number(0), Times("n_()/d_()".asTerm, (0 until variables.length).map(_ => Number(1)).reduceLeft(Times)), Number(0)).reduceLeft(Plus)),
    QE & done)
  def Const(num: BigDecimal, denum: BigDecimal) : TreePolynomial =
    Branch2(Empty(None), Monomial(Coefficient(num, denum, None), (0 until variables.length).map(_ => 0), None), Empty(None),
      Some(rationalLemma(substAny("n_", Number(num))++substAny("d_", Number(denum)))))
  def Const(num: BigDecimal) : TreePolynomial = Branch2(Empty(None), Monomial(Coefficient(num, 1, None), (0 until variables.length).map(_ => 0), None), Empty(None),
    Some(constLemma(substAny("n_", Number(num)))))

  val One : TreePolynomial = Const(1)

  case class Empty(prvO: Option[ProvableSig]) extends TreePolynomial {
    val defaultPrv = zez
    val prv = prvO.getOrElse(defaultPrv)
    override def forgetPrv = Empty(None)
    override def treeSketch: String = "."
  }
  case class Branch2(left: TreePolynomial, value: Monomial, right: TreePolynomial, prvO: Option[ProvableSig]) extends TreePolynomial {
    lazy val defaultPrv = equalReflex(Seq(left.rhs, value.rhs, right.rhs).reduceLeft(Plus))
    // @note detour for "dependent" default argument
    val prv = prvO.getOrElse(defaultPrv)

    override def forgetPrv: TreePolynomial = Branch2(left, value, right, None)
    override def treeSketch: String = "[" + left.treeSketch + ", " + value.powersString + ", " + right.treeSketch + "]"
  }
  case class Branch3(left: TreePolynomial, value1: Monomial, mid: TreePolynomial, value2: Monomial, right: TreePolynomial, prvO: Option[ProvableSig]) extends TreePolynomial {
    lazy val defaultPrv = equalReflex(Seq(left.rhs, value1.rhs, mid.rhs, value2.rhs, right.rhs).reduceLeft(Plus))
    // @note detour for "dependent" default argument
    val prv = prvO.getOrElse(defaultPrv)

    override def forgetPrv: TreePolynomial = Branch3(left, value1, mid, value2, right, None)
    override def treeSketch: String = "{" + left.treeSketch + ", " + value1.powersString + ", " + mid.treeSketch + ", " + value2.powersString + ", " + right.treeSketch + "}"
  }

}