/**
 * Copyright (c) Carnegie Mellon University. CONFIDENTIAL
 * See LICENSE.txt for the conditions of this license.
 */
package edu.cmu.cs.ls.keymaerax.macros

import edu.cmu.cs.ls.keymaerax.macros.DerivedAxiom.ExprPos

import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag
import scala.collection.immutable.HashMap

////////////////////////////////////////////////////////////
// Type structure for central registry of derivation steps
////////////////////////////////////////////////////////////


/** Indicates that the axiom/rule/tactic of the given name could not be found. */
case class AxiomNotFoundException(axiomName: String) extends Exception("Axiom with said name not found: " + axiomName)

/** Central meta-information on a derivation step, which is an axiom, derived axiom, proof rule, or tactic.
 * Provides information such as unique canonical names, internal code names, display information, etc.
 *
 * Each DerivationInfo is either
 *   - [[AxiomInfo]] consisting of builtin [[CoreAxiomInfo]] and derived axioms [[DerivedAxiomInfo]].
 *   - [[AxiomaticRuleInfo]] for builtin axiomatic proof rules.
 *   - [[DerivedRuleInfo]] for derived axiomatic proof rules.
 *   - [[TacticInfo]] for tactics and its various subtypes.
 *
 * Everything consisting of a proved axiom is an [[AxiomInfo]] namely [[CoreAxiomInfo]] and [[DerivedAxiomInfo]].
 * Everything consisting of a Provable is a [[ProvableInfo]], namely [[AxiomInfo]] and [[AxiomaticRuleInfo]] and [[DerivedRuleInfo]].
 *
 * @see [[CoreAxiomInfo]]
 * @see [[DerivedAxiomInfo]]
 * @see [[AxiomaticRuleInfo]]
 * @see [[DerivedRuleInfo]]
 * @see [[TacticInfo]]
 */
object DerivationInfo {
  var _allInfo: List[DerivationInfo] = List()
  def allInfo: List[DerivationInfo] =
    _allInfo match {
      case Nil => throw new Exception("Need to initialize DerivationInfo.allInfo by calling [[]] ")
      case dis => dis
    }

  // @TODO: Hack: derivedAxiom function expects its own derivedaxiominfo to be present during evaluation so that
  // it can look up a stored name rather than computing it. The actual solution is a simple refactor but it touches lots
  // of code so just delay [[value == derivedAxiom(...)]] execution till after info
  def registerDerived[T](value: => T, di: DerivedAxiomInfo): DerivedAxiomInfo = {
    _allInfo = di :: allInfo
    di
  }

  /** code name mapped to derivation information */
  def byCodeName: Map[String, DerivationInfo] =
  /* @todo Decide on a naming convention. Until then, making everything case insensitive */
    allInfo.foldLeft(HashMap.empty[String,DerivationInfo]){case (acc, info) =>
      acc + ((info.codeName, info))
    }

  /** Check whether the given `codeName` is a code name of any of the listed DerivationInfos. */
  def hasCodeName(codeName: String): Boolean = byCodeName.keySet.contains(codeName)

  /** canonical name mapped to derivation information */
  def byCanonicalName: Map[String, DerivationInfo] =
    allInfo.foldLeft(HashMap.empty[String,DerivationInfo]){case (acc, info) =>
      acc + ((info.canonicalName, info))
    }

  /** Throw an AssertionError if id does not conform to the rules for code names. */
  def assertValidIdentifier(id: String): Unit = { assert(id.forall(_.isLetterOrDigit), "valid code name: " + id)}

  /** Retrieve meta-information on an inference by the given code name `codeName` */
  def ofCodeName(codeName:String): DerivationInfo = {
    assert(byCodeName != null, "byCodeName should not be null.")
    assert(codeName != null, "codeName should not be null.")

    byCodeName.getOrElse(codeName, ofBuiltinName(codeName).getOrElse(
      throw new IllegalArgumentException("No such DerivationInfo of identifier " + codeName)
    ))
  }

  /** Retrieve meta-information on a builtin tactic expression by the given `name`. */
  def ofBuiltinName(name: String): Option[DerivationInfo] = {
    val expandPattern = "(expand(?!All).*)|(expandAllDefs)".r
    name match {
      case expandPattern(_*) => Some(new BuiltinInfo(name, SimpleDisplayInfo(name, name)))
      case _ => None
    }
  }

  /** Retrieve meta-information on an inference by the given canonical name `axiomName` */
  def apply(axiomName: String): DerivationInfo = byCanonicalName.getOrElse(axiomName,
    ofBuiltinName(axiomName).getOrElse(throw AxiomNotFoundException(axiomName)))


  /*/** code name mapped to derivation information */
  private val byCodeName: Map[String, DerivationInfo] =
  /* @todo Decide on a naming convention. Until then, making everything case insensitive */
    allInfo.foldLeft(HashMap.empty[String,DerivationInfo]){case (acc, info) =>
      acc + ((info.codeName, info))
    }*/

  /*
  /** canonical name mapped to derivation information */
  private val byCanonicalName: Map[String, DerivationInfo] =
    allInfo.foldLeft(HashMap.empty[String,DerivationInfo]){case (acc, info) =>
      acc + ((info.canonicalName, info))
    }*/
}

/** Typed functions to circumvent type erasure of arguments and return types. */
abstract class TypedFunc[-A: TypeTag, +R: TypeTag] extends (A => R) {
  val retType: TypeTag[_] = scala.reflect.runtime.universe.typeTag[R]
  val argType: TypeTag[_] = scala.reflect.runtime.universe.typeTag[A]
}
/** Creates TypedFunc implicitly, e.g., by ((x: String) => x.length): TypedFunc[String, Int]  */
object TypedFunc {
  implicit def apply[A: TypeTag, R: TypeTag](f: A => R): TypedFunc[A, R] = f match {
    case tf: TypedFunc[A, R]  => tf
    case _ => new TypedFunc[A, R] { final def apply(arg: A): R = f(arg) }
  }
}

sealed trait DerivationInfo {
  /** Canonical full name unique across all derivations (axioms or tactics).
   * For axioms or axiomatic rules this is as declared in
   * [[AxiomBase]], for derived axioms or derived axiomatic rules as in [[DerivedAxioms]],
   * and for [[BelleExpr]] tactics it is identical to their codeName.
   * Canonical names can and will contain spaces and special chars. */
  val canonicalName: String
  /** How to render this inference step for display in a UI */
  val display: DisplayInfo
  /** The unique alphanumeric identifier for this inference step. Cannot contain spaces or special characters. */
  val codeName: String

  /** Specification of inputs (other than positions) to the derivation, along with names to use when displaying in the UI. */
  val inputs: List[ArgInfo] = Nil

  /** Bellerophon tactic implementing the derivation. For non-input tactics this is simply a BelleExpr. For input tactics
   * it is (curried) function which accepts the inputs and produces a BelleExpr. */
  //def theExpr: Any

  /** Number of positional arguments to the derivation. Can be 0, 1 or 2.
   *   - 0 means this inference cannot be positioned but applies to the whole sequent.
   *   - 1 means this inference will be applied at one position.
   *   - 2 means this inference will be applied with two positions as input (e.g., use info at -2 to simplify 1). */
  val numPositionArgs: Int = 0
  /** Whether the derivation expects the caller to provide it with a way to generate invariants */
  val needsGenerator: Boolean = false
  /** Whether the derivation makes internal steps that are useful for users to see. */
  val revealInternalSteps: Boolean = false

  override def toString: String = "DerivationInfo(" + canonicalName + "," + codeName + ")"
}

// provables

/** Meta-Information for a (derived) axiom or (derived) axiomatic rule
 * @see [[AxiomInfo]]
 * @see [[AxiomaticRuleInfo]]
 * @see [[DerivedRuleInfo]] */
trait ProvableInfo extends DerivationInfo {
  /** The [[ProvableSig]] representing this (derived) axiom or (derived) axiomatic rule. Needs to be [[Any]] to avoid
   * type dependency between separate modules. Implicit method [[provable: ProvableSig]] in keymaerax-core project
   * recovers intended type */
  //val theProvable: Option[Any]
  /** `true` indicates that the key of this axiom/axiomatic proof rule can be matched linearly [[LinearMatcher]].
   * For completeness, this linearity declaration must be consistent with the default key from [[AxiomIndex.axiomFor()]].
   * @see [[LinearMatcher]] */
  def linear: Boolean
}

/** Storable derivation info (e.g., as lemmas).
 * @see [[DerivedAxiomInfo]]
 * @see [[DerivedRuleInfo]]
 */
trait StorableInfo extends DerivationInfo {
  val storedName: String = DerivedAxiomInfo.toStoredName(codeName)
}

// axioms

/** Meta-Information for an axiom or derived axiom
 * @see [[edu.cmu.cs.ls.keymaerax.btactics.AxiomIndex]] */
trait AxiomInfo extends ProvableInfo {
  /** The valid formula that this axiom represents */
  //def formula: Formula
}

/** Meta-Information for an axiom from the prover core
 * @see [[edu.cmu.cs.ls.keymaerax.core.AxiomBase]]
 * @see [[DerivedAxiomInfo]]
 *   [[theExpr]] should be [[Unit => DependentPositionTactic]], correct type recovered in keymaerax-core wrapper
 */
case class CoreAxiomInfo(  override val canonicalName:String
                         , override val display: DisplayInfo
                         , override val codeName: String
                         , val unifier: Symbol
                         , val theExpr: Unit => Any)
  extends AxiomInfo {
  DerivationInfo.assertValidIdentifier(codeName)
  override val numPositionArgs = 1
  override def linear: Boolean = ('linear == unifier)
}

/** Information for a derived axiom proved from the core.
 * @see [[edu.cmu.cs.ls.keymaerax.btactics.DerivedAxioms]]
 * @see [[CoreAxiomInfo]]
 * @TODO: Enforce theExpr : Unit => DependentPositionTactic
 * */
case class DerivedAxiomInfo(  override val canonicalName: String
                            , override val display: DisplayInfo
                            , override val codeName: String
                            , val unifier: Symbol
                            , theExpr: Unit => Any
                            , val displayLevel: Symbol = 'all
                            , val theKey: ExprPos = Nil
                            , val theRecursor: List[ExprPos] = Nil
                            )
  extends AxiomInfo with StorableInfo {
  override val storedName: String = DerivedAxiomInfo.toStoredName(codeName)
  DerivationInfo.assertValidIdentifier(codeName)
  //def belleExpr: BelleExpr = codeName by ((pos: Position, _: Sequent) => expr()(pos))
  //override lazy val formula: Formula =
  //DerivedAxioms.derivedAxiomOrRule(canonicalName).conclusion.succ.head
  //override lazy val provable:ProvableSig = DerivedAxioms.derivedAxiomOrRule(canonicalName)
  override val numPositionArgs = 1
  override def linear: Boolean = ('linear == unifier)
}

// axiomatic proof rules

/** Information for an axiomatic proof rule
 * @see [[edu.cmu.cs.ls.keymaerax.core.AxiomBase]]
 * @see [[DerivedRuleInfo]] */
case class AxiomaticRuleInfo(override val canonicalName:String, override val display: DisplayInfo, override val codeName: String, theExpr: Unit => Any)
  extends ProvableInfo {
  // lazy to avoid circular initializer call
  //private[this] lazy val expr = TactixLibrary.by(provable, codeName)
  DerivationInfo.assertValidIdentifier(codeName)
  //def belleExpr = expr
  //lazy val provable: ProvableSig = ProvableSig.rules(canonicalName)
  override val numPositionArgs = 0
  //@note Presently, only nonlinear shapes in core axiomatic rules in [[edu.cmu.cs.ls.keymaerax.core.AxiomBase.loadAxiomaticRules]]
  override val linear: Boolean = false
}


/** Information for a derived rule proved from the core
 * @see [[edu.cmu.cs.ls.keymaerax.btactics.DerivedAxioms]]
 * @see [[AxiomaticRuleInfo]] */
case class DerivedRuleInfo(override val canonicalName:String, override val display: DisplayInfo, override val codeName: String, val theExpr: Unit => Any)
  extends ProvableInfo with StorableInfo {
  DerivationInfo.assertValidIdentifier(codeName)
  //def belleExpr = expr()
  //lazy val provable: ProvableSig = DerivedAxioms.derivedAxiomOrRule(canonicalName)
  override val numPositionArgs = 0
  //@note Presently, mostly nonlinear shapes in derived axiomatic rules
  override val linear: Boolean = false
}


// tactics

/** Meta-information on builtin tactic expressions (expand etc.). */
class BuiltinInfo(  override val codeName: String
                  , override val display: DisplayInfo
                  , override val needsGenerator: Boolean = false
                  , override val revealInternalSteps: Boolean = false)
  extends DerivationInfo {
  //def belleExpr: BelleExpr = BelleParser(codeName)
  val canonicalName: String = codeName
}

/** Meta-information on a tactic performing a proof step (or more) */
class TacticInfo(  override val codeName: String
                 , override val display: DisplayInfo
                 , val theExpr: Unit => Any
                 , needsTool: Boolean = false
                 , override val needsGenerator: Boolean = false
                 , override val revealInternalSteps: Boolean = false)
  extends DerivationInfo {
  DerivationInfo.assertValidIdentifier(codeName)
  //def belleExpr = expr()
  val canonicalName = codeName
}

case class PositionTacticInfo(override val codeName: String, override val display: DisplayInfo, override val theExpr: Unit => Any,
                              needsTool: Boolean = false, override val needsGenerator: Boolean = false,
                              override val revealInternalSteps: Boolean = false)
  extends TacticInfo(codeName, display, theExpr, needsTool, needsGenerator, revealInternalSteps) {
  override val numPositionArgs = 1
}

case class TwoPositionTacticInfo(override val codeName: String, override val display: DisplayInfo, override val theExpr: Unit => Any, needsTool: Boolean = false, override val needsGenerator: Boolean = false)
  extends TacticInfo(codeName, display, theExpr, needsTool, needsGenerator) {
  override val numPositionArgs = 2
}

case class InputTacticInfo(override val codeName: String, override val display: DisplayInfo, override val inputs:List[ArgInfo], expr: Unit => TypedFunc[_, _], needsTool: Boolean = false,
                           override val needsGenerator: Boolean = false, override val revealInternalSteps: Boolean = false)
  extends TacticInfo(codeName, display, expr, needsTool, needsGenerator, revealInternalSteps)

case class InputPositionTacticInfo(override val codeName: String, override val display: DisplayInfo,
                                   override val inputs:List[ArgInfo], expr: Unit => TypedFunc[_,_], needsTool: Boolean = false,
                                   override val needsGenerator: Boolean = false, override val revealInternalSteps: Boolean = false)
  extends TacticInfo(codeName, display, expr, needsTool, needsGenerator, revealInternalSteps) {
  override val numPositionArgs = 1
}

case class InputTwoPositionTacticInfo(override val codeName: String, override val display: DisplayInfo, override val inputs:List[ArgInfo], expr: Unit => TypedFunc[_, _], needsTool: Boolean = false, override val needsGenerator: Boolean = false)
  extends TacticInfo(codeName, display, expr, needsTool, needsGenerator) {
  override val numPositionArgs = 2
}

object DerivedAxiomInfo {
  /** Retrieve meta-information on an axiom by the given canonical name `axiomName` */
  def locate(axiomName: String): Option[DerivedAxiomInfo] =
    DerivationInfo.byCanonicalName(axiomName) match {
      case info: DerivedAxiomInfo => Some(info)
      case _ => None
    }
  /** Retrieve meta-information on an axiom by the given canonical name `axiomName` */
  def apply(axiomName: String): DerivedAxiomInfo = {
    DerivationInfo.byCanonicalName(axiomName) match {
      case info: DerivedAxiomInfo => info
      case info => throw new Exception("Derivation \"" + info.canonicalName + "\" is not a derived axiom")
    }
  }

  def toStoredName(codeName: String): String = codeName.toLowerCase
  def allInfo:List[DerivedAxiomInfo] =  DerivationInfo.allInfo.filter(_.isInstanceOf[DerivedAxiomInfo]).map(_.asInstanceOf[DerivedAxiomInfo])
}

// axiomatic proof rules

object DerivedRuleInfo {
  /** Retrieve meta-information on a rule by the given canonical name `ruleName` */
  def locate(ruleName: String): Option[DerivedRuleInfo] =
    DerivationInfo.byCanonicalName(ruleName) match {
      case info: DerivedRuleInfo => Some(info)
      case _ => None
    }
  /** Retrieve meta-information on a rule by the given canonical name `ruleName` */
  def apply(ruleName: String): DerivedRuleInfo =
    DerivationInfo.byCanonicalName(ruleName) match {
      case info: DerivedRuleInfo => info
      case info => throw new Exception("Derivation \"" + info.canonicalName + "\" is not a derived rule")
    }

  def allInfo:List[DerivedRuleInfo] =  DerivationInfo.allInfo.filter(_.isInstanceOf[DerivedRuleInfo]).map(_.asInstanceOf[DerivedRuleInfo])
}

// tactics

object TacticInfo {
  def apply(tacticName: String): TacticInfo =
    DerivationInfo.byCodeName(tacticName) match {
      case info:TacticInfo => info
      case info => throw new Exception("Derivation \"" + info.canonicalName + "\" is not a tactic")
    }
}

////////////////////////////////////////////////////////////
// Companion objects for projections of DerivationInfo registry
////////////////////////////////////////////////////////////

// provables

object ProvableInfo {
  /** Retrieve meta-information on a (derived) axiom or (derived) axiomatic rule by the given canonical name `name` */
  def locate(name: String): Option[ProvableInfo] =
    DerivationInfo(name) match {
      case info: ProvableInfo => Some(info)
      case _ => None
    }
  /** Retrieve meta-information on a (derived) axiom or (derived) axiomatic rule by the given canonical name `name` */
  def apply(name: String): ProvableInfo = {
    val res =  DerivationInfo(name)
    res match {
      case info: ProvableInfo => info
      case info => throw new Exception("Derivation \"" + info.canonicalName + "\" is not an axiom or axiomatic rule, whether derived or not.")
    }
  }

  /** True if ProvableInfo with `storedName` exists, false otherwise. */
  def existsStoredName(storedName: String): Boolean =
    DerivationInfo.allInfo.exists({case si: StorableInfo => si.storedName == storedName case _ => false})

  /** Retrieve meta-information on an inference by the given stored name `storedName` */
  def ofStoredName(storedName: String): ProvableInfo = {
    DerivationInfo.allInfo.find({case si: StorableInfo => si.storedName == storedName case _ => false}) match {
      case Some(info: ProvableInfo) => info
      case Some(info) => throw new Exception("Derivation \"" + info.canonicalName + "\" is not an axiom or axiomatic rule, whether derived or not.")
      case _ => throw new Exception("Derivation \"" + storedName + "\" is not a derived axiom or rule.")
    }
  }

  def allInfo:List[ProvableInfo] =  DerivationInfo.allInfo.filter(_.isInstanceOf[ProvableInfo]).map(_.asInstanceOf[ProvableInfo])
}

// axioms

object AxiomInfo {
  /** Retrieve meta-information on an axiom by the given canonical name `axiomName` */
  def apply(axiomName: String): AxiomInfo =
    DerivationInfo(axiomName) match {
      case info:AxiomInfo => info
      case info => throw new Exception("Derivation \"" + info.canonicalName + "\" is not an axiom")
    }

  /** Retrieve meta-information on an axiom by the given code name `codeName` */
  def ofCodeName(codeName: String): AxiomInfo =
    DerivationInfo.ofCodeName(codeName) match {
      case info:AxiomInfo => info
      case info => throw new Exception("Derivation \"" + info.canonicalName + "\" is not an axiom")
    }

  def allInfo:List[AxiomInfo] =  DerivationInfo.allInfo.filter(_.isInstanceOf[AxiomInfo]).map(_.asInstanceOf[AxiomInfo])
}



object CoreAxiomInfo {
  /** Retrieve meta-information on a core axiom by the given canonical name `axiomName` */
  def apply(axiomName: String): CoreAxiomInfo =
    DerivationInfo(axiomName) match {
      case info:CoreAxiomInfo => info
      case info => throw new Exception("Derivation \"" + info.canonicalName + "\" is not a core axiom")
    }

  /** Retrieve meta-information on a core axiom by the given code name `codeName` */
  def ofCodeName(codeName: String): CoreAxiomInfo =
    DerivationInfo.ofCodeName(codeName) match {
      case info:CoreAxiomInfo => info
      case info => throw new Exception("Derivation \"" + info.canonicalName + "\" is not an axiom")
    }

  def allInfo:List[CoreAxiomInfo] =  DerivationInfo.allInfo.filter(_.isInstanceOf[CoreAxiomInfo]).map(_.asInstanceOf[CoreAxiomInfo])
}

