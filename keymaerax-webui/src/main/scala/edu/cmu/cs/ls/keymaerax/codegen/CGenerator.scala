/**
 * Copyright (c) Carnegie Mellon University. CONFIDENTIAL
 * See LICENSE.txt for the conditions of this license.
 */
package edu.cmu.cs.ls.keymaerax.codegen

import edu.cmu.cs.ls.keymaerax.infrastruct.ExpressionTraversal.{ExpressionTraversalFunction, StopTraversal}
import edu.cmu.cs.ls.keymaerax.codegen.CFormulaTermGenerator._
import edu.cmu.cs.ls.keymaerax.codegen.CGenerator._
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.infrastruct.{ExpressionTraversal, PosInExpr}

object CGenerator {
  /** Prints a file header */
  def printHeader(modelName: String): String =
    s"""/**************************
       | *${if(modelName.nonEmpty) " " + modelName + ".c" else ""}
       | * Generated by KeYmaera X
       | **************************/
       |
       |""".stripMargin

  /** Prints include statements. */
  val INCLUDE_STATEMENTS: String =
    """#include <math.h>
      |#include <stdbool.h>
      |
      |""".stripMargin

  /** Prints the parameters struct declaration. */
  def printParameterDeclaration(parameters: Set[NamedSymbol]): String = printStructDeclaration("parameters", parameters)

  /** Prints the state variables struct declaration. */
  def printStateDeclaration(stateVars: Set[BaseVariable]): String = printStructDeclaration("state", stateVars)

  /** Prints the input (non-deterministically assigned variables) struct declaration. */
  def printInputDeclaration(inputs: Set[BaseVariable]): String = printStructDeclaration("input", inputs)

  def printVerdictDeclaration() = s"typedef struct verdict { int id; long double val; } verdict;\n\n"

  /**
    * Returns a set of names (excluding names in `exclude` and interpreted functions) that are immutable parameters of the
    * expression `expr`. */
  def getParameters(expr: Expression, exclude: Set[BaseVariable]): Set[NamedSymbol] =
    StaticSemantics.symbols(expr)
      .filter({
        case Function("abs" | "exp" | "sin" | "cos", None, Real, Real, true) => false
        case Function("min" | "max", None, Tuple(Real, Real), Real, true) => false
        case Function(name, _, Unit, _, _) => !exclude.exists(v => v.name == name.stripSuffix("post"))
        case BaseVariable(name, _, _) => !exclude.exists(v => v.name == name.stripSuffix("post"))
        case _ => false //@note any other function or differential symbol
      })

  /** Returns a set of names whose values are chosen nondeterministically in the program `expr` (empty if `expr` is not
    * a program). */
  def getInputs(expr: Expression): Set[BaseVariable] = expr match {
    case prg: Program =>
      val inputs = scala.collection.mutable.Set[BaseVariable]()
      ExpressionTraversal.traverse(new ExpressionTraversalFunction {
        override def preP(p: PosInExpr, e: Program): Either[Option[StopTraversal], Program] = e match {
          case AssignAny(v: BaseVariable) => inputs += v; Left(None)
          case _ => Left(None)
        }
      }, prg)
      inputs.toSet
    case _ => Set()
  }
}

/**
  * C++ code generator that prints a file header, include statements, declarations, and the output of `bodyGenerator`.
  * @author Ran Ji
  * @author Stefan Mitsch
  */
class CGenerator(bodyGenerator: CodeGenerator) extends CodeGenerator {
  /** Generate C Code for given expression using the data type cDataType throughout and the input list of variables */
  override def apply(expr: Expression, stateVars: Set[BaseVariable], inputVars: Set[BaseVariable], fileName: String): (String, String) =
    generateMonitoredCtrlCCode(expr, stateVars, inputVars, fileName)

  /** Generates a monitor `expr` that switches between a controller and a fallback controller depending on the monitor outcome. */
  private def generateMonitoredCtrlCCode(expr: Expression, stateVars: Set[BaseVariable], inputVars: Set[BaseVariable], fileName: String) : (String, String) = {
    val names = StaticSemantics.symbols(expr).map(nameIdentifier)
    require(names.intersect(RESERVED_NAMES).isEmpty, "Unexpected reserved C names encountered: " + names.intersect(RESERVED_NAMES).mkString(","))
    val parameters = getParameters(expr, stateVars)

    val (bodyBody, bodyDefs) = bodyGenerator(expr, stateVars, inputVars, fileName)

    (printHeader(fileName) +
      INCLUDE_STATEMENTS +
      printParameterDeclaration(parameters) +
      printStateDeclaration(stateVars) +
      printInputDeclaration(inputVars) +
      printVerdictDeclaration +
      bodyDefs, bodyBody)
  }

  private val RESERVED_NAMES = Set("main", "Main")
}