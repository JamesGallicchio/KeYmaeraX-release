/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/
/**
 * HyDRA API Requests
  *
  * @author Nathan Fulton
 * @author Ran Ji
 */
package edu.cmu.cs.ls.keymaerax.hydra

import edu.cmu.cs.ls.keymaerax.bellerophon._
import edu.cmu.cs.ls.keymaerax.hydra.SQLite.SQLiteDB
import edu.cmu.cs.ls.keymaerax.parser._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.btactics._
import edu.cmu.cs.ls.keymaerax.btactics.DerivationInfoRegistry
import edu.cmu.cs.ls.keymaerax.infrastruct.Augmentors._
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.bellerophon.parser.{BelleParser, BellePrettyPrinter, HackyInlineErrorMsgPrinter}
import edu.cmu.cs.ls.keymaerax.infrastruct.ExpressionTraversal.{ExpressionTraversalFunction, StopTraversal}
import edu.cmu.cs.ls.keymaerax.tools._
import edu.cmu.cs.ls.keymaerax.tools.ext._
import spray.json._
import spray.json.DefaultJsonProtocol._
import java.io._
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.concurrent.{FutureTask, TimeUnit, TimeoutException}
import java.util.{Calendar, Locale}

import edu.cmu.cs.ls.keymaerax.{Configuration, UpdateChecker}
import edu.cmu.cs.ls.keymaerax.bellerophon.IOListeners.CollectProgressListener
import edu.cmu.cs.ls.keymaerax.btactics.Generator.Generator
import edu.cmu.cs.ls.keymaerax.btactics.InvariantGenerator.GenProduct
import edu.cmu.cs.ls.keymaerax.pt.{ElidingProvable, ProvableSig}

import scala.io.Source
import scala.collection.immutable._
import scala.collection.mutable
import edu.cmu.cs.ls.keymaerax.btactics.cexsearch.{BoundedDFS, ProgramSearchNode, SearchNode}
import edu.cmu.cs.ls.keymaerax.btactics.helpers.DifferentialHelper
import edu.cmu.cs.ls.keymaerax.codegen.{CControllerGenerator, CGenerator, CMonitorGenerator}
import edu.cmu.cs.ls.keymaerax.infrastruct._
import edu.cmu.cs.ls.keymaerax.lemma.{Lemma, LemmaDBFactory}
import edu.cmu.cs.ls.keymaerax.macros._
import DerivationInfoAugmentors._
import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXArchiveParser.{InputSignature, ParsedArchiveEntry, Signature}
import edu.cmu.cs.ls.keymaerax.tools.ext.{Mathematica, QETacticTool, TestSynthesis, WolframScript, Z3}
import edu.cmu.cs.ls.keymaerax.tools.install.ToolConfiguration
import edu.cmu.cs.ls.keymaerax.tools.qe.{DefaultSMTConverter, KeYmaeraToMathematica}
import org.apache.logging.log4j.scala.Logging

import scala.annotation.tailrec
import scala.util.Try

/**
 * A Request should handle all expensive computation as well as all
 * possible side-effects of a request (e.g. updating the database), but should
 * not modify the internal state of the HyDRA server (e.g. do not update the
 * event queue).
 *
 * Requests objects should do work after getResultingUpdates is called,
 * not during object construction.
 *
 * Request.getResultingUpdates might be run from a new thread.
 */
sealed trait Request extends Logging {
  /** Checks read/write/registered access. Additional checks by overriding doPermission. */
  final def permission(t: SessionToken): Boolean = (t match {
    case t: ReadonlyToken => this.isInstanceOf[ReadRequest]
    case _ => true
  }) && doPermission(t)

  /** Override to provide additional permission checks. */
  protected def doPermission(t: SessionToken): Boolean = true

  final def getResultingResponses(t: SessionToken): List[Response] = {
    if (!permission(t)) new PossibleAttackResponse("Permission denied")::Nil
    else {
      assert(permission(t), "Permission denied but still responses queried (see completeRequest)")
      try {
        theSession = SessionManager.session(t)
        resultingResponses()
      } catch {
        //@note Avoids "Boxed Error" without error message by wrapping unchecked exceptions here.
        //      The web server translates exception into 500 response, the web UI picks them up in the error alert dialog
        // assert, ensures
        case a: AssertionError => throw new Exception(
          "We're sorry, an internal safety check was violated, which may point to a bug. The safety check reports " + a.getMessage, a)
        // require
        case e: IllegalArgumentException => throw new Exception(
          "We're sorry, an internal safety check was violated, which may point to a bug. The safety check reports " + e.getMessage, e)
      }
    }
  }

  private var theSession: SessionManager.Session = _
  def session: SessionManager.Session = theSession

  def resultingResponses(): List[Response]

  def currentDate(): String = {
    val format = new SimpleDateFormat("d-M-y")
    format.format(Calendar.getInstance().getTime)
  }
}

trait ReadRequest
trait RegisteredOnlyRequest
trait WriteRequest extends RegisteredOnlyRequest

/**
  * @todo we don't always check that the username is in fact associated with the other data that's touched by a request.
  *       For example, openProof might not insist that the proofId actually belongs to the associated userId in the request.
  *       The best solution to this in the long term is a re-design of the API, probably.
  * @param username The username of the current user.
  */
abstract class UserRequest(username: String) extends Request {
  override protected def doPermission(t: SessionToken): Boolean = t belongsTo username
}

/** A proof session storing information between requests. */
case class ProofSession(proofId: String, invGenerator: Generator[GenProduct], defs: KeYmaeraXArchiveParser.Declaration)

abstract class UserModelRequest(db: DBAbstraction, username: String, modelId: String) extends UserRequest(username) {
  override final def resultingResponses(): List[Response] = {
    //@todo faster query for existence
    if (db.getModel(modelId).userId != username) new PossibleAttackResponse("Permission denied") :: Nil
    else doResultingResponses()
  }

  protected def doResultingResponses(): List[Response]
}

abstract class UserProofRequest(db: DBAbstraction, username: String, proofId: String) extends UserRequest(username) {
  override final def resultingResponses(): List[Response] = {
    Try(proofId.toInt).toOption match {
      case Some(_) =>
        //@todo faster query for existence
        if (HyDRAServerConfig.isHosted && db.getProofInfo(proofId).modelId.isDefined && !db.userOwnsProof(username, proofId)) {
          new PossibleAttackResponse("Permission denied") :: Nil
        } else doResultingResponses()
      case None => new ErrorResponse("The user interface lost track of the proof, please try reloading the page.") :: Nil //@note Web UI bug)
    }
  }

  protected def doResultingResponses(): List[Response]
}

abstract class LocalhostOnlyRequest() extends Request {
  override protected def doPermission(t: SessionToken): Boolean = !HyDRAServerConfig.isHosted //@todo change this to a literal false prior to deployment.
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Users
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class CreateUserRequest(db: DBAbstraction, username: String, password: String, mode: String) extends Request with WriteRequest {
  override def resultingResponses(): List[Response] = {
    db.getUser(username) match {
      case Some(user) => new LoginResponse(false, user, None) ::  Nil
      case None =>
        db.createUser(username, password, mode)
        db.getUser(username) match {
          case Some(newUser) => new LoginResponse(true, newUser, Some(SessionManager.add(newUser))) ::  Nil
          case None => new ErrorResponse("Failed to create user " + username) :: Nil
        }
    }
  }
}

class SetDefaultUserRequest(db: DBAbstraction, username: String, password: String, useDefault: Boolean) extends LocalhostOnlyRequest with WriteRequest {
  override def resultingResponses(): List[Response] = {
    if (useDefault) {
      if (db.checkPassword(username, password)) {
        Configuration.set(Configuration.Keys.DEFAULT_USER, username, saveToFile = true)
        Configuration.set(Configuration.Keys.USE_DEFAULT_USER, "true", saveToFile = true)
        BooleanResponse(flag = true) :: Nil
      } else new ErrorResponse("Failed to set default user") :: Nil
    } else {
      Configuration.remove(Configuration.Keys.DEFAULT_USER, saveToFile = true)
      Configuration.set(Configuration.Keys.USE_DEFAULT_USER, "false", saveToFile = true)
      BooleanResponse(flag = true) :: Nil
    }
  }
}

class LocalLoginRequest(db: DBAbstraction, username: String, password: String) extends LocalhostOnlyRequest with ReadRequest {
  override def resultingResponses(): List[Response] = {
    if (Configuration.getOption(Configuration.Keys.USE_DEFAULT_USER).contains ("true") && username == "local") {
      Configuration.getOption(Configuration.Keys.DEFAULT_USER) match {
        case Some(username) => db.getUser(username) match {
          case Some(user) =>
            val sessionToken = Some(SessionManager.add(user))
            new LoginResponse(true, user, sessionToken) :: Nil
          case None => DefaultLoginResponse(triggerRegistration = true) :: Nil
        }
        case None => DefaultLoginResponse(triggerRegistration = true) :: Nil
      }
    } else {
      val check = db.checkPassword(username, password)
      db.getUser(username) match {
        case Some(user) =>
          val sessionToken =
            if (check) Some(SessionManager.add(user))
            else None
          new LoginResponse(check, user, sessionToken) :: Nil
        case None => new ErrorResponse("Unable to login user " + username
          + ". Please double-check user name and password, or register a new user.") :: Nil
      }
    }
  }
}

class LoginRequest(db: DBAbstraction, username: String, password: String) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val check = db.checkPassword(username, password)
    db.getUser(username) match {
      case Some(user) =>
        val sessionToken =
          if (check) Some(SessionManager.add(user))
          else None
        new LoginResponse(check, user, sessionToken) :: Nil
      case None => new ErrorResponse("Unable to login user " + username
        + ". Please double-check user name and password, or register a new user.") :: Nil
    }
  }
}

class ProofsForUserRequest(db: DBAbstraction, userId: String) extends UserRequest(userId) with ReadRequest {
  def resultingResponses(): List[Response] = {
    val proofs = db.getProofsForUser(userId).filterNot(_._1.temporary).map(proof =>
      (proof._1, "loaded"/*KeYmaeraInterface.getTaskLoadStatus(proof._1.proofId.toString).toString.toLowerCase*/))
    new ProofListResponse(proofs) :: Nil
  }
}

class UserLemmasRequest(db: DBAbstraction, userId: String) extends UserRequest(userId) with ReadRequest {
  def resultingResponses(): List[Response] = {
    val proofs = db.getProofsForUser(userId).filterNot(_._1.temporary).filter(_._1.closed).
      groupBy(_._1.modelId).map(_._2.head).map(proof => (proof._1, proof._1.modelId.map(db.getModel))).toList
    new UserLemmasResponse(proofs) :: Nil
  }
}

class UpdateProofNameRequest(db : DBAbstraction, userId: String, proofId : String, newName : String) extends UserProofRequest(db, userId, proofId) with WriteRequest {
  override protected def doResultingResponses(): List[Response] = {
    db.updateProofName(proofId, newName)
    new UpdateProofNameResponse(proofId, newName) :: Nil
  }
}

class FailedRequest(userId: String, msg: String, cause: Throwable = null) extends UserRequest(userId) {
  def resultingResponses(): List[Response] = { new ErrorResponse(msg, cause) :: Nil }
}

class CounterExampleRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, assumptions: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  def allFnToVar(fml: Formula, fn: Function): Formula = {
    fml.find(t => t match {
        case FuncOf(func, _) if fn.sort == Real => func == fn
        case PredOf(func, arg) if fn.sort == Bool && arg != Nothing => func == fn
        case _ => false }) match {
      case Some((_, e: Term)) => allFnToVar(fml.replaceAll(e, Variable(fn.name, fn.index, Real)), fn)
      case Some((_, e: Formula)) => allFnToVar(fml.replaceAll(e, PredOf(Function(fn.name, fn.index, Unit, Bool), Nothing)), fn) //@todo beware of name clashes
      case None => fml
    }
  }

  def findCounterExample(fml: Formula, cexTool: CounterExampleTool): Option[Map[NamedSymbol, Expression]] = {
    val signature = StaticSemantics.signature(fml).filter({
      case Function(_, _, _, _, false) => true case _ => false }).map(_.asInstanceOf[Function])
    val lmf = signature.foldLeft[Formula](fml)((f, t) => allFnToVar(f, t))
    cexTool.findCounterExample(lmf) match {
      case Some(cex) => Some(cex.map({case (k, v) => signature.find(s => s.name == k.name && s.index == k.index).getOrElse(k) -> v }))
      case None => None
    }
  }

  override protected def doResultingResponses(): List[Response] = {
    val json = assumptions.parseJson.asJsObject.fields.get("additional")
    val additionalAssumptions: Option[Formula] = try {
      json.map(_.convertTo[String].asFormula)
    } catch {
      case ex: ParseException => return ParseErrorResponse("Expected assumptions as a formula, but got " + json.getOrElse("<empty>"),
        ex.expect, ex.found, ex.getDetails, ex.loc, ex) :: Nil
    }

    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId) match {
      case None => new ErrorResponse("Unknown node " + nodeId)::Nil
      case Some(node) =>
        //@note not a tactic because we don't want to change the proof tree just by looking for counterexamples
        def nonfoError(sequent: Sequent) = {
          val nonFOAnte = sequent.ante.filterNot(_.isFOL)
          val nonFOSucc = sequent.succ.filterNot(_.isFOL)
          new CounterExampleResponse("cex.nonfo", (nonFOSucc ++ nonFOAnte).head) :: Nil
        }

        @tailrec
        def getCex(node: ProofTreeNode, cexTool: CounterExampleTool): List[Response] = {
          val sequent = node.goal.get
          if (sequent.isFOL) {
            if (StaticSemantics.symbols(sequent).isEmpty) {
              //@note counterexample on false (e.g., after QE on invalid formula)
              node.parent match {
                case Some(parent) => getCex(parent, cexTool)
                case None => new CounterExampleResponse("cex.none") :: Nil
              }
            } else {
              val skolemized = TactixLibrary.proveBy(sequent,
                SaturateTactic(TactixLibrary.alphaRule | TactixLibrary.allR('R) | TactixLibrary.existsL('L)))
              val fml = skolemized.subgoals.map(_.toFormula).reduceRight(And)
              val withAssumptions = additionalAssumptions match {
                case Some(a) => Imply(a, fml)
                case None => fml
              }
              try {
                findCounterExample(withAssumptions, cexTool) match {
                  //@todo return actual sequent, use collapsiblesequentview to display counterexample
                  case Some(cex) => new CounterExampleResponse("cex.found", fml, cex) :: Nil
                  case None => new CounterExampleResponse("cex.none") :: Nil
                }
              } catch {
                case ex: ToolException => new ErrorResponse("Error executing counterexample tool", ex) :: Nil
              }
            }
          } else {
            val fml = sequent.toFormula
            /* TODO: Case on this instead */
            val qeTool: QETacticTool = ToolProvider.qeTool().get
            val snode: SearchNode = ProgramSearchNode(fml)(qeTool)
            val search = new BoundedDFS(10)
            try {
              search(snode) match {
                case None => nonfoError(sequent)
                case Some(cex) => new CounterExampleResponse("cex.found", fml, cex.map) :: Nil
              }
            } catch {
              // Counterexample generation is quite hard for, e.g. ODEs, so expect some cases to be unimplemented.
              // When that happens, just tell the user they need to simplify the formula more.
              case _ : NotImplementedError => nonfoError(sequent)
            }
          }
        }

        try {
          node.goal match {
            case Some(sequent) if sequent.isFOL => ToolProvider.cexTool() match {
                case Some(cexTool) => getCex(node, cexTool)
                case None => new CounterExampleResponse("cex.notool") :: Nil
              }
            case Some(sequent) => sequent.succ.find({ case Box(_: ODESystem, _) => true case _ => false }) match {
              case Some(Box(ode: ODESystem, post)) => ToolProvider.invGenTool() match {
                case Some(tool) => tool.refuteODE(ode, sequent.ante, post) match {
                  case None => new CounterExampleResponse("cex.none") :: Nil
                  case Some(cex) => new CounterExampleResponse("cex.found", sequent.toFormula, cex) :: Nil
                }
                case None => new CounterExampleResponse("cex.notool") :: Nil
              }
            }
            case None => new CounterExampleResponse("cex.none") :: Nil
          }
        } catch {
          case _: MathematicaComputationAbortedException => new CounterExampleResponse("cex.timeout") :: Nil
        }
    }
  }
}

class ODEConditionsRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId) match {
      case None => new ErrorResponse("Unknown node " + nodeId) :: Nil
      case Some(node) =>
        try {
          node.goal match {
            case Some(sequent) => sequent.succ.find({ case Box(_: ODESystem, _) => true case _ => false }) match {
              case Some(Box(ode: ODESystem, post)) => ToolProvider.invGenTool() match {
                case Some(tool) =>
                  val (sufficient, necessary) = tool.genODECond(ode, sequent.ante, post)
                  new ODEConditionsResponse(sufficient, necessary) :: Nil
                case None => new ODEConditionsResponse(Nil, Nil) :: Nil
              }
              case None => new ErrorResponse("ODE system needed to search for ODE conditions, but succedent does not contain an ODE system or ODE system may not be at top level. Please perform additional proof steps until ODE system is at top level.") :: Nil
            }
            case None => new ErrorResponse("ODE system needed to search for ODE conditions, but goal is empty.") :: Nil
          }
        } catch {
          case _: MathematicaComputationAbortedException => new ErrorResponse("ODE conditions search timeout.") :: Nil
        }
    }
  }
}

class PegasusCandidatesRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId) match {
      case None => new ErrorResponse("Unknown node " + nodeId) :: Nil
      case Some(node) =>
        try {
          node.goal match {
            case Some(sequent) => sequent.succ.find({ case Box(_: ODESystem, _) => true case _ => false }) match {
              case Some(Box(ode: ODESystem, post)) if post.isFOL => ToolProvider.invGenTool() match {
                case Some(tool) =>
                  val invs = tool.invgen(ode, sequent.ante, post)
                  new PegasusCandidatesResponse(invs) :: Nil
                case None => new PegasusCandidatesResponse(Nil) :: Nil
              }
              case Some(Box(_, post)) if !post.isFOL => new ErrorResponse("Post-condition in FOL is needed to search for invariants; please perform further proof steps until the post-condition of the ODE is a formula in first-order logic.") :: Nil
              case None => new ErrorResponse("ODE system needed to search for invariant candidates, but succedent does not contain an ODE system or ODE system may not be at top level. Please perform additional proof steps until ODE system is at top level.") :: Nil
            }
            case None => new ErrorResponse("ODE system needed to search for invariant candidates, but goal is empty.") :: Nil
          }
        } catch {
          case _: MathematicaComputationAbortedException => new ErrorResponse("ODE invariant search timeout.") :: Nil
        }
    }
  }
}

class SetupSimulationRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String) extends UserProofRequest(db, userId, proofId) with RegisteredOnlyRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId) match {
      case None => new ErrorResponse("Unknown node " + nodeId) :: Nil
      case Some(node) =>
        //@note not a tactic because we don't want to change the proof tree just by simulating
        val sequent = node.goal.get
        val fml = sequent.toFormula match {
          case Imply(True, succ) => succ //@todo really? below we error response if not an implication
          case f => f
        }
        if (ToolProvider.odeTool().isDefined) fml match {
          case Imply(initial, b@Box(prg, _)) =>
            // all symbols because we need frame constraints for constants
            val vars = (StaticSemantics.symbols(prg) ++ StaticSemantics.symbols(initial)).filter(_.isInstanceOf[BaseVariable])
            val Box(prgPre, _) = vars.foldLeft[Formula](b)((b, v) => b.replaceAll(v, Variable("pre" + v.name, v.index, v.sort)))
            val stateRelEqs = vars.map(v => Equal(v.asInstanceOf[Term], Variable("pre" + v.name, v.index, v.sort))).reduceRightOption(And).getOrElse(True)
            val simSpec = Diamond(solveODEs(prgPre), stateRelEqs)
            new SetupSimulationResponse(addNonDetInitials(initial, vars), transform(simSpec)) :: Nil
          case _ => new ErrorResponse("Simulation only supported for formulas of the form initial -> [program]safe") :: Nil
        } else new ErrorResponse("No simulation tool available, please configure Mathematica") :: Nil
    }
  }

  private def addNonDetInitials(initial: Formula, vars: Set[NamedSymbol]): Formula = {
    val nonDetInitials = vars -- StaticSemantics.freeVars(initial).symbols
    nonDetInitials.foldLeft(initial)((f, v) => And(f, Equal(v.asInstanceOf[Term], v.asInstanceOf[Term])))
  }

  private def transform(simSpec: Diamond): Formula = {
    val stateRelation = TactixLibrary.proveBy(simSpec, TactixLibrary.chase(3, 3, (e: Expression) => e match {
      // no equational assignments
      case Box(Assign(_,_),_) => Ax.assignbAxiom :: Ax.assignbup :: Nil
      case Diamond(Assign(_,_),_) => Ax.assigndAxiom :: Ax.assigndup :: Nil
      // remove loops
      case Diamond(Loop(_), _) => Ax.loopApproxd :: Nil
      //@note: do nothing, should be gone already
      case Diamond(ODESystem(_, _), _) => Nil
      case _ => AxIndex.axiomsFor(e)
    })('R))
    assert(stateRelation.subgoals.size == 1 &&
      stateRelation.subgoals.head.ante.isEmpty &&
      stateRelation.subgoals.head.succ.size == 1, "Simulation expected to result in a single formula")
    stateRelation.subgoals.head.succ.head
  }

  private def solveODEs(prg: Program): Program = ExpressionTraversal.traverse(new ExpressionTraversalFunction() {
    override def preP(p: PosInExpr, e: Program): Either[Option[StopTraversal], Program] = e match {
      case ODESystem(ode, evoldomain) =>
        Right(Compose(Test(evoldomain), solve(ode, evoldomain)))
      case _ => Left(None)
    }
  }, prg).get

  private def solve(ode: DifferentialProgram, evoldomain: Formula): Program = {
    val iv: Map[Variable, Variable] =
      DifferentialHelper.getPrimedVariables(ode).map(v => v -> Variable(v.name + "0", v.index, v.sort)).toMap
    val time: Variable = Variable("t_", None, Real)
    //@note replace initial values with original variable, since we turn them into assignments
    val solution = replaceFree(ToolProvider.odeTool().get.odeSolve(ode, time, iv).get, iv.map(_.swap))
    val flatSolution = FormulaTools.conjuncts(solution).
      sortWith((f, g) => StaticSemantics.symbols(f).size < StaticSemantics.symbols(g).size)
    Compose(
      flatSolution.map({ case Equal(v: Variable, r) => Assign(v, r) }).reduceRightOption(Compose).getOrElse(Test(True)),
      Test(evoldomain))
  }

  private def replaceFree(f: Formula, vars: Map[Variable, Variable]) = {
    vars.keySet.foldLeft[Formula](f)((b, v) => b.replaceFree(v, vars(v)))
  }
}

class SimulationRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, initial: Formula, stateRelation: Formula, steps: Int, n: Int, stepDuration: Term) extends UserProofRequest(db, userId, proofId) with RegisteredOnlyRequest {
  override protected def doResultingResponses(): List[Response] = {
    def replaceFuncs(fml: Formula) = ExpressionTraversal.traverse(new ExpressionTraversalFunction() {
      override def preT(p: PosInExpr, e: Term): Either[Option[StopTraversal], Term] = e match {
        case FuncOf(Function(name, idx, Unit, Real, false), _) => Right(BaseVariable(name, idx))
        case _ => Left(None)
      }
    }, fml)

    ToolProvider.simulationTool() match {
      case Some(s) =>
        val varsStateRelation = replaceFuncs(stateRelation).get
        val varsInitial = replaceFuncs(initial).get
        val timedStateRelation = varsStateRelation.replaceFree(Variable("t_"), stepDuration)
        val simulation = s.simulate(varsInitial, timedStateRelation, steps, n)
        new SimulationResponse(simulation, stepDuration) :: Nil
      case _ => new ErrorResponse("No simulation tool configured, please setup Mathematica") :: Nil
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System Configuration
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class KyxConfigRequest(db: DBAbstraction) extends LocalhostOnlyRequest with ReadRequest {
  val newline = "\n"
  override def resultingResponses() : List[Response] = {
    // keymaera X version
    val kyxConfig = "KeYmaera X version: " + VERSION + newline +
      "Java version: " + System.getProperty("java.runtime.version") + " with " + System.getProperty("sun.arch.data.model") + " bits" + newline +
      "OS: " + System.getProperty("os.name") + " " + System.getProperty("os.version") + newline +
      "LinkName: " + Configuration.getOption(Configuration.Keys.MATHEMATICA_LINK_NAME) + newline +
      "jlinkLibDir: " + Configuration.getOption(Configuration.Keys.MATHEMATICA_JLINK_LIB_DIR)
    new KyxConfigResponse(kyxConfig) :: Nil
  }
}

class KeymaeraXVersionRequest() extends Request with ReadRequest {
  override def resultingResponses() : List[Response] = {
    val keymaeraXVersion = VERSION
    val (upToDate, latestVersion) = UpdateChecker.getVersionStatus match {
      case Some((upd, lv)) => (Some(upd), Some(lv))
      case _ => (None, None)
    }
    new KeymaeraXVersionResponse(keymaeraXVersion, upToDate, latestVersion) :: Nil
  }
}

class ConfigureMathematicaRequest(db: DBAbstraction, toolName: String,
                                  linkName: String, jlinkLibFileName: String, jlinkTcpip: String)
    extends LocalhostOnlyRequest with WriteRequest {
  private def isLinkNameCorrect(linkNameFile: java.io.File): Boolean = {
    linkNameFile.getName == "MathKernel" || linkNameFile.getName == "MathKernel.exe"
  }

  private def isJLinkLibFileCorrect(jlinkFile: java.io.File, jlinkLibDir : java.io.File): Boolean = {
    (jlinkFile.getName == "libJLinkNativeLibrary.jnilib" || jlinkFile.getName == "JLinkNativeLibrary.dll" ||
      jlinkFile.getName == "libJLinkNativeLibrary.so") && jlinkLibDir.exists() && jlinkLibDir.isDirectory
  }

  override def resultingResponses(): List[Response] = {
    //check to make sure the indicated files exist and point to the correct files.
    val linkNameFile = new java.io.File(linkName)
    val jlinkLibFile = new java.io.File(jlinkLibFileName)
    val jlinkLibDir: java.io.File = jlinkLibFile.getParentFile
    val linkNameExists = isLinkNameCorrect(linkNameFile) && linkNameFile.exists()
    val jlinkLibFileExists = isJLinkLibFileCorrect(jlinkLibFile, jlinkLibDir) && jlinkLibFile.exists()
    var linkNamePrefix = linkNameFile
    var jlinkLibNamePrefix = jlinkLibFile

    if (!linkNameExists) {
      // look for the largest prefix that does exist
      while (!linkNamePrefix.exists && linkNamePrefix.getParent != null) {
        linkNamePrefix = new java.io.File(linkNamePrefix.getParent)
      }
    }
    if (!jlinkLibFileExists) {
      // look for the largest prefix that does exist
      while (!jlinkLibNamePrefix.exists && jlinkLibNamePrefix.getParent != null) {
        jlinkLibNamePrefix = new java.io.File(jlinkLibNamePrefix.getParent)
      }
    }
    if (!linkNameExists || !jlinkLibFileExists) {
      new ConfigureMathematicaResponse(
        if (linkNamePrefix.exists()) linkNamePrefix.toString else "",
        if (jlinkLibNamePrefix.exists()) jlinkLibNamePrefix.toString else "", false) :: Nil
    } else {
      ToolProvider.shutdown()
      Configuration.set(Configuration.Keys.QE_TOOL, toolName)
      val provider = toolName match {
        case "wolframengine" =>
          Configuration.set(Configuration.Keys.WOLFRAMENGINE_TCPIP, jlinkTcpip)
          Configuration.set(Configuration.Keys.WOLFRAMENGINE_LINK_NAME, linkNameFile.getAbsolutePath)
          Configuration.set(Configuration.Keys.WOLFRAMENGINE_JLINK_LIB_DIR, jlinkLibDir.getAbsolutePath)
          ToolProvider.initFallbackZ3(new WolframEngineToolProvider(ToolConfiguration.config(toolName)), "Wolfram Engine")
        case "mathematica" =>
          Configuration.set(Configuration.Keys.MATH_LINK_TCPIP, jlinkTcpip)
          Configuration.set(Configuration.Keys.MATHEMATICA_LINK_NAME, linkNameFile.getAbsolutePath)
          Configuration.set(Configuration.Keys.MATHEMATICA_JLINK_LIB_DIR, jlinkLibDir.getAbsolutePath)
          ToolProvider.initFallbackZ3(new MathematicaToolProvider(ToolConfiguration.config(toolName)), "Mathematica")
      }
      ToolProvider.setProvider(provider)
      new ConfigureMathematicaResponse(linkNameFile.getAbsolutePath, jlinkLibDir.getAbsolutePath, true) :: Nil
    }
  }
}

class GetMathematicaConfigSuggestionRequest(db : DBAbstraction) extends LocalhostOnlyRequest with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val allSuggestions = ToolConfiguration.mathematicaSuggestion()
    val (suggestionFound, suggestion) = allSuggestions.find(s => new java.io.File(s.kernelPath + s.kernelName).exists &&
        new java.io.File(s.jlinkPath + s.jlinkName).exists) match {
      case Some(s) => (true, s)
      case None => (false, allSuggestions.head) // use the first configuration as suggestion when nothing else matches
    }

    val os = System.getProperty("os.name")
    val jvmBits = System.getProperty("sun.arch.data.model")
    new MathematicaConfigSuggestionResponse(os, jvmBits, suggestionFound, suggestion, allSuggestions) :: Nil
  }
}

class GetWolframEngineConfigSuggestionRequest(db: DBAbstraction) extends LocalhostOnlyRequest with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val allSuggestions = ToolConfiguration.wolframEngineSuggestion()
    val (suggestionFound, suggestion) = allSuggestions.find(s => new java.io.File(s.kernelPath + s.kernelName).exists &&
      new java.io.File(s.jlinkPath + s.jlinkName).exists) match {
      case Some(s) => (true, s)
      case None => (false, allSuggestions.head) // use the first configuration as suggestion when nothing else matches
    }

    val os = System.getProperty("os.name")
    val jvmBits = System.getProperty("sun.arch.data.model")
    new MathematicaConfigSuggestionResponse(os, jvmBits, suggestionFound, suggestion, allSuggestions) :: Nil
  }
}

class GetWolframScriptConfigSuggestionRequest(db: DBAbstraction) extends LocalhostOnlyRequest with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val os = System.getProperty("os.name")
    val jvmBits = System.getProperty("sun.arch.data.model")
    try {
      val we = new WolframScript()
      val version = we.getVersion
      we.shutdown()
      new MathematicaConfigSuggestionResponse(os, jvmBits, true,
        ToolConfiguration.ConfigSuggestion(version.major + "." + version.minor + "." + version.revision, "", "", "",
        ""), Nil) :: Nil
    } catch {
      case _: Throwable =>
        new MathematicaConfigSuggestionResponse(os, jvmBits, false,
          ToolConfiguration.ConfigSuggestion("", "", "", "", ""), Nil) :: Nil
    }
  }
}

class SystemInfoRequest(db: DBAbstraction) extends LocalhostOnlyRequest with ReadRequest {
  override def resultingResponses(): List[Response] = {
    new SystemInfoResponse(
      System.getProperty("os.name"),
      System.getProperty("os.version"),
      System.getProperty("java.home"),
      System.getProperty("java.vendor"),
      System.getProperty("java.version"),
      System.getProperty("sun.arch.data.model")) :: Nil
  }
}

class LicensesRequest() extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val reader = this.getClass.getResourceAsStream("/license/tools_licenses")
    // StringOps for JDK 11 compatibility
    val lines = (Source.fromInputStream(reader).mkString: StringOps).lines.toList
    val header = lines.head
    val licenseStartPos = header.indexOf("License")
    val licenses = lines.tail.tail.map(l => l.splitAt(licenseStartPos)).map({case (tool, license) =>
        JsObject(
          "tool" -> JsString(tool.trim),
          "license" -> JsString(license.trim)
        )
    })
    new PlainResponse("licenses" -> JsArray(licenses:_*)) :: Nil
  }
}

class GetToolRequest(db: DBAbstraction) extends LocalhostOnlyRequest with ReadRequest {
  override def resultingResponses(): List[Response] = {
    //@todo more/different tools
    val toolName = Configuration(Configuration.Keys.QE_TOOL)
    ToolProvider.tool(toolName) match {
      case Some(tool) =>
        val initialized = tool.isInitialized
        if (initialized) new KvpResponse("tool", toolName) :: Nil
        else new ErrorResponse(toolName + " is not initialized. Please double-check the configuration paths.") :: Nil
      case _ => new ErrorResponse(toolName + " failed to initialize. Please reselect the desired tool and double-check the configuration paths. Temporarily switched to fallback tools " + ToolProvider.tools().map(_.name).mkString(",")) :: Nil
    }
  }
}

class SetToolRequest(db: DBAbstraction, tool: String) extends LocalhostOnlyRequest with WriteRequest {
  override def resultingResponses(): List[Response] = {
    //@todo more/different tools
    if (tool != "mathematica" && tool != "z3" && tool != "wolframengine" && tool != "wolframscript") new ErrorResponse("Unknown tool " + tool + ", expected either 'mathematica' or 'z3' or 'wolframengine'")::Nil
    else {
      assert(tool == "mathematica" || tool == "z3" || tool == "wolframengine" || tool == "wolframscript", "Expected either Mathematica or Z3 or Wolfram Engine tool")
      ToolProvider.shutdown()
      val config = ToolConfiguration.config(tool)
      try {
        val provider: Option[ToolProvider] = tool match {
          case "mathematica" =>
            if (new java.io.File(config.getOrElse("linkName", "")).exists &&
                new java.io.File(config.getOrElse("libDir", "")).exists) {
              if (Configuration.contains(Configuration.Keys.MATHEMATICA_LINK_NAME) &&
                Configuration.contains(Configuration.Keys.MATHEMATICA_JLINK_LIB_DIR)) {
                Some(new MultiToolProvider(new MathematicaToolProvider(config) :: new Z3ToolProvider() :: Nil))
              } else None
            } else {
              ToolProvider.setProvider(new Z3ToolProvider())
              None
            }
          case "wolframengine" =>
            if (new java.io.File(config.getOrElse("linkName", "")).exists &&
                new java.io.File(config.getOrElse("libDir", "")).exists) {
              if (Configuration.contains(Configuration.Keys.WOLFRAMENGINE_LINK_NAME) &&
                Configuration.contains(Configuration.Keys.WOLFRAMENGINE_JLINK_LIB_DIR) &&
                Configuration.contains(Configuration.Keys.WOLFRAMENGINE_TCPIP)) {
                Some(new MultiToolProvider(new WolframEngineToolProvider(config) :: new Z3ToolProvider() :: Nil))
              } else None
            } else {
              ToolProvider.setProvider(new Z3ToolProvider())
              None
            }
          case "wolframscript" => Some(new MultiToolProvider(new WolframScriptToolProvider() :: new Z3ToolProvider() :: Nil))
          case "z3" => Some(new Z3ToolProvider())
          case _ => ToolProvider.setProvider(new NoneToolProvider); None
        }
        provider match {
          case Some(p) =>
            Configuration.set(Configuration.Keys.QE_TOOL, tool)
            ToolProvider.setProvider(p)
          case _ => // nothing to do
        }
        new ToolConfigStatusResponse(tool, provider.isDefined) :: Nil
      } catch {
        case ex: Throwable if tool == "mathematica" => new ErrorResponse("Error initializing " + tool + ". Please double-check the configuration paths, that the license is valid (e.g., start Mathematica and type $LicenseExpirationDate, check that license server is reachable, if used), and that the Java JVM 32/64bit fits your operating system.", ex) :: Nil
        case ex: Throwable if tool == "wolframengine" => new ErrorResponse("Error initializing " + tool + ". Please double-check the configuration paths, that the license is valid and the computer is online for license checking. If Wolfram Engine remains unavailable and/or keeps crashing KeYmaera X, please run Wolfram Engine to update the license information (check by running $LicenseExpirationDate in Wolfram Engine) prior to starting KeYmaera X. Also make sure that the Java JVM 32/64bit fits your operating system.", ex) :: Nil
        case ex: Throwable => new ErrorResponse("Error initializing " + tool + ". Please double-check the configuration paths and that the Java JVM 32/64bit fits your operating system.", ex) :: Nil
      }
    }
  }
}

class GetMathematicaConfigurationRequest(db: DBAbstraction, toolName: String) extends LocalhostOnlyRequest with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val osName = System.getProperty("os.name").toLowerCase(Locale.ENGLISH)
    val jlinkLibFile = {
      if (osName.contains("win")) "JLinkNativeLibrary.dll"
      else if (osName.contains("mac")) "libJLinkNativeLibrary.jnilib"
      else if (osName.contains("nix") || osName.contains("nux") || osName.contains("aix")) "libJLinkNativeLibrary.so"
      else "Unknown"
    }
    toolName match {
      case "mathematica" if Configuration.contains(Configuration.Keys.MATHEMATICA_LINK_NAME) && Configuration.contains(Configuration.Keys.MATHEMATICA_JLINK_LIB_DIR) =>
          new MathematicaConfigurationResponse(
            Configuration(Configuration.Keys.MATHEMATICA_LINK_NAME),
            Configuration(Configuration.Keys.MATHEMATICA_JLINK_LIB_DIR) + File.separator + jlinkLibFile,
            Configuration.getOption(Configuration.Keys.MATH_LINK_TCPIP).getOrElse("")
          ) :: Nil
      case "wolframengine" if Configuration.contains(Configuration.Keys.WOLFRAMENGINE_LINK_NAME) && Configuration.contains(Configuration.Keys.WOLFRAMENGINE_JLINK_LIB_DIR) =>
        new MathematicaConfigurationResponse(
          Configuration(Configuration.Keys.WOLFRAMENGINE_LINK_NAME),
          Configuration(Configuration.Keys.WOLFRAMENGINE_JLINK_LIB_DIR) + File.separator + jlinkLibFile,
          Configuration.getOption(Configuration.Keys.WOLFRAMENGINE_TCPIP).getOrElse("")
        ) :: Nil
      case _ => new MathematicaConfigurationResponse("", "", "") :: Nil
    }
  }
}

class GetUserThemeRequest(db: DBAbstraction, userName: String) extends UserRequest(userName) with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val config = db.getConfiguration(userName).config
    new PlainResponse(
      "themeCss" -> config.getOrElse("themeCss", "\"app\"").parseJson,
      "themeFontSize" -> config.getOrElse("themeFontSize", "14").parseJson,
      "renderMargins" -> config.getOrElse("renderMargins", "[40,80]").parseJson
    ) :: Nil
  }
}

/** Sets the UI theme. @note ReadRequest allows changing theme in guest mode for presentation purposes. */
class SetUserThemeRequest(db: DBAbstraction, userName: String, themeCss: String, themeFontSize: String, renderMargins: String)
    extends UserRequest(userName) with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val config = db.getConfiguration(userName)
    db.updateConfiguration(new ConfigurationPOJO(userName,
      config.config.updated("themeCss", themeCss).
        updated("themeFontSize", themeFontSize).
        updated("renderMargins", renderMargins)))
    new PlainResponse(
      "themeCss" -> themeCss.parseJson,
      "themeFontSize" -> themeFontSize.parseJson,
      "renderMargins" -> renderMargins.parseJson
    ) :: Nil
  }
}


class MathematicaConfigStatusRequest(db: DBAbstraction) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = {
    ToolProvider.tool("mathematica") match {
      case Some(_) =>
        new ToolConfigStatusResponse("mathematica",
          Configuration.contains(Configuration.Keys.MATHEMATICA_LINK_NAME) &&
          Configuration.contains(Configuration.Keys.MATHEMATICA_JLINK_LIB_DIR) &&
          ToolProvider.tool("mathematica").isDefined) :: Nil
      case None => new ToolConfigErrorResponse("mathematica", "Mathematica could not be started; please double-check the configured paths and make sure you have a valid license (if you use a license server, make sure it is reachable). Temporarily using " + ToolProvider.tools().map(_.name).mkString(",") + " with potentially limited functionality.") :: Nil
    }
  }
}

class WolframEngineConfigStatusRequest(db: DBAbstraction) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = {
    ToolProvider.tool("wolframEngine") match {
      case Some(_) =>
        new ToolConfigStatusResponse("wolframengine",
          Configuration.contains(Configuration.Keys.WOLFRAMENGINE_LINK_NAME) &&
          Configuration.contains(Configuration.Keys.WOLFRAMENGINE_JLINK_LIB_DIR) &&
          Configuration.contains(Configuration.Keys.WOLFRAMENGINE_TCPIP)) :: Nil
      case None => new ToolConfigErrorResponse("wolframengine", "Wolfram Engine could not be started; please double-check the configured paths and make sure you are online for license checking. Temporarily using " + ToolProvider.tools().map(_.name).mkString(",") + " with potentially limited functionality.") :: Nil
    }

  }
}

class WolframScriptConfigStatusRequest(db: DBAbstraction) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = new ToolConfigStatusResponse("wolframscript", true) :: Nil
}

class ToolStatusRequest(db: DBAbstraction, toolId: String) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = {
    ToolProvider.tool(toolId) match {
      case Some(t: ToolOperationManagement) => new ToolStatusResponse(toolId, t.getAvailableWorkers) :: Nil
      case Some(_) => new ToolStatusResponse(toolId, -1) :: Nil
      case None => new ToolConfigErrorResponse(toolId, "Tool could not be started; please check KeYmaera X -> Preferences. Temporarily using " + ToolProvider.tools().map(_.name).mkString(",") + " with potentially limited functionality.") :: Nil
    }

  }
}

//@todo Detect closed connections and request timeouts server-side
class CancelToolRequest() extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val allCancelled = ToolProvider.tools().map(_.cancel()).reduce(_ && _)
    BooleanResponse(flag = allCancelled) :: Nil
  }
}

class Z3ConfigStatusRequest(db : DBAbstraction) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = new ToolConfigStatusResponse("z3", true) :: Nil
}

class RestartToolRequest(db: DBAbstraction, toolId: String) extends LocalhostOnlyRequest {
  override def resultingResponses(): List[Response] = {
    ToolProvider.tool(toolId) match {
      case Some(t: Tool) =>
        t.restart()
        new GenericOKResponse :: Nil
      case _ => new ErrorResponse(s"Restarting failed: unknown tool '$toolId'. Please check the tool configuration.") :: Nil
    }

  }
}

class TestToolConnectionRequest(db: DBAbstraction, toolId: String) extends LocalhostOnlyRequest {
  override def resultingResponses(): List[Response] = {
    ToolProvider.tool(toolId) match {
      case Some(t: QETool) =>
        val simpleQeTask = new FutureTask[Either[Formula, Throwable]](() =>
          try {
            Left(t.quantifierElimination("1+2=3".asFormula))
          } catch {
            case e: Throwable => Right(e)
          })
        new Thread(simpleQeTask).start()
        try {
          val result = simpleQeTask.get(1, TimeUnit.SECONDS)
          if (result.isLeft && result.left.get == "true".asFormula) new GenericOKResponse :: Nil
          else if (result.isLeft && result.left.get != "true".asFormula) new ErrorResponse("Testing connection failed: unexpected result " + result.left.get + " for test 2+3=5") :: Nil
          else /* result.isRight */ new ErrorResponse("Testing connection failed", result.right.get) :: Nil
        } catch {
          case _: TimeoutException =>
            new ErrorResponse("Testing connection failed: tool is not responding. Please restart KeYmaera X.") :: Nil
        }
      case Some(t: Tool) => new ErrorResponse(s"Testing connection failed: do not know how to test '${t.getClass}' tool") :: Nil
      case _ => new ErrorResponse(s"Testing connection failed: unknown tool '$toolId'. Please check the tool configuration.") :: Nil
    }

  }
}

/** List of all predefined tutorials that can directly be imported from the KeYmaera X web UI.
  * List of tutorials, in order of display.
  *
  * @param db
  * @param userId
  */
class ListExamplesRequest(db: DBAbstraction, userId: String) extends UserRequest(userId) with ReadRequest {
  override def resultingResponses(): List[Response] = {
    //@todo read from the database/some web page?
    //@note Learner's mode Level=0, Industry mode Level=1
    val examples =
    new ExamplePOJO(6, "Textbook",
      "LFCPS 2018 Textbook",
      "",
      "classpath:/keymaerax-projects/lfcps/lfcps.kyx",
      "/examples/tutorials/lfcps-examples.png", 0) ::
    new ExamplePOJO(6, "MOD19",
      "Marktoberdorf 2019 Tutorial Examples",
      //"/keymaerax-projects/lfcps-turorial/README.md",
      "",
      "classpath:/keymaerax-projects/lfcps-tutorial/lfcps-tutorial.kyx",
      "/examples/tutorials/cpsweek/cpsweek.png", 1) ::
    new ExamplePOJO(5, "POPL 2019 Tutorial",
      "Programming CPS With Proofs",
      //"/keymaerax-projects/popltutorial/README.md",
      "",
      "classpath:/keymaerax-projects/popltutorial/popltutorial.kyx",
      "/examples/tutorials/cpsweek/cpsweek.png", 1) ::
      new ExamplePOJO(4, "DLDS",
        "Dynamic Logic for Dynamical Systems Examples",
        //"/keymaerax-projects/dlds/README.md",
        "",
        "classpath:/keymaerax-projects/dlds/dlds.kya",
        "/examples/tutorials/cpsweek/cpsweek.png", 1) ::
      new ExamplePOJO(0, "STTT Tutorial",
        "Automated stop sign braking for cars",
        "/dashboard.html?#/tutorials",
        "classpath:/examples/tutorials/sttt/sttt.kyx",
        "/examples/tutorials/sttt/sttt.png", 1) ::
      new ExamplePOJO(1, "CPSWeek 2016 Tutorial",
        "Proving ODEs",
        "http://www.ls.cs.cmu.edu/KeYmaeraX/KeYmaeraX-tutorial.pdf",
        "classpath:/examples/tutorials/cpsweek/cpsweek.kyx",
        "/examples/tutorials/cpsweek/cpsweek.png", 1) ::
      new ExamplePOJO(2, "FM 2016 Tutorial",
        "Tactics and Proofs",
        "/dashboard.html?#/tutorials",
        "classpath:/examples/tutorials/fm/fm.kyx",
        "/examples/tutorials/fm/fm.png", 1) ::
      new ExamplePOJO(3, "Beginner's Tutorial",
        "Feature Tour Tutorial",
        "/dashboard.html?#/tutorials",
        "classpath:/examples/tutorials/basic/basictutorial.kyx",
        "/examples/tutorials/fm/fm.png", 0) ::
//        new ExamplePOJO(3, "POPL 2019 Tutorial",
//          "Programming CPS With Proofs",
//          //"/keymaerax-projects/popltutorial/README.md",
//          "",
//          "classpath:/keymaerax-projects/popltutorial/popltutorial.kyx",
//          "/examples/tutorials/cpsweek/cpsweek.png", 1) ::
        Nil

    db.getUser(userId) match {
      case Some(user) => new ListExamplesResponse(examples.filter(_.level <= user.level)) :: Nil
      case None => new ErrorResponse("Unable to retrieve examples. Unknown user " + userId) :: Nil
    }

  }
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Models
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Creates a model from a formula without variable declarations.
  * Separate from CreateModelRequest so that we don't end up swallowing parse errors or returning the wrong parse error. */
class CreateModelFromFormulaRequest(db: DBAbstraction, userId: String, nameOfModel: String, formula: String) extends UserRequest(userId) with WriteRequest {
  private var createdId : Option[String] = None

  def resultingResponses(): List[Response] = try {
    KeYmaeraXParser(formula) match {
      case _: Formula =>
        if(db.getModelList(userId).map(_.name).contains(nameOfModel))
          new BooleanResponse(false, Some("A model with name " + nameOfModel + " already exists, please choose a different name")) :: Nil
        else {
          createdId = db.createModel(userId, nameOfModel, formula, currentDate()).map(_.toString)
          new BooleanResponse(createdId.isDefined) :: Nil
        }
      case t => new ErrorResponse("Expected a formula, but got the " + t.kind + " " + formula) :: Nil
    }
  } catch {
    case e: ParseException => new ParseErrorResponse(e.msg, e.expect, e.found, e.getDetails, e.loc, e) :: Nil
  }

  def getModelId: String = createdId match {
    case Some(s) => s
    case None => throw new IllegalStateException("Requested created model ID before calling resultingResponses, or else an error occurred during creation.")
  }
}

class UpdateModelRequest(db: DBAbstraction, userId: String, modelId: String, name: String, title: String,
                         description: String, content: String) extends UserRequest(userId) with WriteRequest {
  private def emptyToOption(s: String): Option[String] = if (s.isEmpty) None else Some(s)

  def resultingResponses(): List[Response] = {
    val modelInfo = db.getModel(modelId)
    if (db.getProofsForModel(modelId).forall(_.stepCount == 0)) {
      if (KeYmaeraXArchiveParser.isExercise(content)) {
        db.updateModel(modelId.toInt, name, emptyToOption(title), emptyToOption(description), emptyToOption(content))
        BooleanResponse(flag = true) :: Nil
      } else try {
        KeYmaeraXArchiveParser.parseAsProblemOrFormula(content) match {
          case fml: Formula =>
            val newContent = RequestHelper.augmentDeclarations(content, fml)
            db.updateModel(modelId.toInt, name, emptyToOption(title), emptyToOption(description), emptyToOption(newContent))
            BooleanResponse(flag = true) :: Nil
          case t => new ErrorResponse("Expected a model formula, but got a file with a " + t.kind) :: Nil
        }
      } catch {
        case e: ParseException => ParseErrorResponse(e.msg, e.expect, e.found, e.getDetails, e.loc, e) :: Nil
      }
    } else new ErrorResponse("Unable to update model " + modelId + " because it has " + modelInfo.numAllProofSteps + " proof steps") :: Nil
  }
}

class UploadArchiveRequest(db: DBAbstraction, userId: String, archiveText: String, modelName: Option[String]) extends UserRequest(userId) with WriteRequest {
  def resultingResponses(): List[Response] = {
    try {
      val parsedArchiveEntries = KeYmaeraXArchiveParser.parse(archiveText)

      //@note archive parser augments a plain formula with definitions and flags it with name '<undefined>'
      val archiveEntries =
        if (parsedArchiveEntries.size == 1 && parsedArchiveEntries.head.name == "<undefined>") {
          parsedArchiveEntries.head.copy(name = modelName.getOrElse("undefined")) :: Nil
        } else parsedArchiveEntries

      val (failedModels, succeededModels) = archiveEntries.foldLeft((List[String](), List[(String, Int)]()))({ case ((failedImports, succeededImports), entry) =>
        val result = DatabasePopulator.importModel(db, userId, prove=false)(DatabasePopulator.toTutorialEntry(entry))
        (failedImports ++ result.right.toSeq, succeededImports ++ result.left.toSeq)
      })
      if (failedModels.isEmpty) {
        if (archiveEntries.size == 1) {
          ModelUploadResponse(Some(succeededModels.head._2.toString), None) :: Nil
        } else BooleanResponse(flag = true) :: Nil
      } else throw new Exception("Failed to import the following models\n" + failedModels.mkString("\n") +
        "\nSucceeded importing:\n" + succeededModels.mkString("\n") +
        "\nModel import may have failed because of model name clashed. Try renaming the failed models in the archive to names that do not yet exist in your model list.")
    } catch {
      //@todo adapt parse error positions (are relative to problem inside entry)
      case e: ParseException => ParseErrorResponse(e.msg, e.expect, e.found, e.getDetails, e.loc, e) :: Nil
    }
  }
}

class DeleteModelRequest(db: DBAbstraction, userId: String, modelId: String) extends UserModelRequest(db, userId, modelId) with WriteRequest {
  override def doResultingResponses(): List[Response] = {
    val id = Integer.parseInt(modelId)
    //db.getProofsForModel(id).foreach(proof => TaskManagement.forceDeleteTask(proof.proofId.toString))
    val success = db.deleteModel(id)
    BooleanResponse(success) :: Nil
  }
}

class DeleteAllModelsRequest(db: DBAbstraction, userId: String) extends UserRequest(userId) with WriteRequest {
  override def resultingResponses(): List[Response] = {
    val allModels = db.getModelList(userId).map(_.modelId)
    allModels.foreach(db.deleteModel)
    BooleanResponse(flag = true) :: Nil
  }
}

class DeleteModelProofStepsRequest(db: DBAbstraction, userId: String, modelId: String) extends UserModelRequest(db, userId, modelId) with WriteRequest {
  override def doResultingResponses(): List[Response] = {
    val deletedSteps = db.getProofsForModel(modelId).map(p => db.deleteProofSteps(p.proofId)).sum
    BooleanResponse(deletedSteps > 0) :: Nil
  }
}

class DeleteProofRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with WriteRequest {
  override protected def doResultingResponses(): List[Response] = {
    //TaskManagement.forceDeleteTask(proofId)
    val success = db.deleteProof(Integer.parseInt(proofId))
    BooleanResponse(success) :: Nil
  }
}

class GetModelListRequest(db : DBAbstraction, userId : String) extends UserRequest(userId) with ReadRequest {
  def resultingResponses(): List[Response] = {
    new ModelListResponse(db.getModelList(userId).filterNot(_.temporary)) :: Nil
  }
}

class GetModelRequest(db : DBAbstraction, userId : String, modelId : String) extends UserRequest(userId) with ReadRequest {
  private val model: ModelPOJO = db.getModel(modelId)
  insist(model.userId == userId, s"model $modelId does not belong to $userId")
  def resultingResponses(): List[Response] = {
    new GetModelResponse(model) :: Nil
  }
}

class GetModelTacticRequest(db : DBAbstraction, userId : String, modelId : String) extends UserRequest(userId) with ReadRequest {
  def resultingResponses(): List[Response] = {
    val model = db.getModel(modelId)
    new GetModelTacticResponse(model) :: Nil
  }
}

class AddModelTacticRequest(db : DBAbstraction, userId : String, modelId : String, tactic: String) extends UserRequest(userId) with WriteRequest {
  def resultingResponses(): List[Response] = {
    val tacticId = db.addModelTactic(modelId, tactic)
    new BooleanResponse(tacticId.isDefined) :: Nil
  }
}

class ModelPlexMandatoryVarsRequest(db: DBAbstraction, userId: String, modelId: String) extends UserRequest(userId) with RegisteredOnlyRequest {
  def resultingResponses(): List[Response] = {
    val model = db.getModel(modelId)
    val modelFml = KeYmaeraXArchiveParser.parseAsProblemOrFormula(model.keyFile)
    new ModelPlexMandatoryVarsResponse(model, StaticSemantics.boundVars(modelFml).symbols.filter(_.isInstanceOf[BaseVariable])) :: Nil
  }
}

class ModelPlexRequest(db: DBAbstraction, userId: String, modelId: String, artifact: String, monitorKind: String,
                       monitorShape: String, conditionKind: String,
                       additionalVars: List[String]) extends UserRequest(userId) with RegisteredOnlyRequest {
  def resultingResponses(): List[Response]  = {
    val model = db.getModel(modelId)
    val modelFml = KeYmaeraXArchiveParser.parseAsProblemOrFormula(model.keyFile)
    val vars: Set[BaseVariable] = (StaticSemantics.boundVars(modelFml).symbols ++ additionalVars.map(_.asVariable)).
      filter(_.isInstanceOf[BaseVariable]).map(_.asInstanceOf[BaseVariable])

    artifact match {
      case "controller" => createController(model, modelFml, vars)
      case "monitor" => createMonitor(model, modelFml, vars)
      case "sandbox" => createSandbox(model, modelFml, vars)
    }
  }

  private def createController(model: ModelPOJO, modelFml: Formula, vars: Set[BaseVariable]): List[Response] = modelFml match {
    case Imply(_, Box(prg, _)) => conditionKind match {
      case "kym" =>
        val ctrlPrg = ExpressionTraversal.traverse(new ExpressionTraversalFunction() {
          override def preP(p: PosInExpr, prg: Program): Either[Option[StopTraversal], Program] = prg match {
            case _: ODESystem => Right(Test("true".asFormula))
            case _ => Left(None)
          }
        }, prg).get
        new ModelPlexArtifactResponse(model, ctrlPrg) :: Nil
      case "c" =>
        val controller = (new CGenerator(new CControllerGenerator()))(prg, vars, CGenerator.getInputs(prg))

        val code = s"""
           |${CGenerator.printHeader(model.name)}
           |${controller._1}
           |${controller._2}
           |
           |int main() {
           |  /* control loop stub */
           |  parameters params = { .A=1.0 };
           |  while (true) {
           |    state current; /* read sensor values, e.g., = { .x=0.0 }; */
           |    state input;   /* resolve non-deterministic assignments, e.g., = { .x=0.5 }; */
           |    state post = ctrlStep(current, params, input);
           |    /* hand post actuator set values to actuators */
           |  }
           |  return 0;
           |}
           |""".stripMargin

        new ModelPlexArtifactCodeResponse(model, code) :: Nil
    }
    case _ => new ErrorResponse("Unsupported shape, expected assumptions -> [{ctrl;ode}*]safe, but got " + modelFml.prettyString) :: Nil
  }

  private def createSandbox(model: ModelPOJO, modelFml: Formula, stateVars: Set[BaseVariable]): List[Response] = modelFml match {
    case Imply(_, Box(prg, _)) =>
      conditionKind match {
        case "kym" =>
          //@todo specific formula
          new ModelPlexArtifactResponse(model, "pre := curr; ctrl; if (monitorSatisfied()) { skip; } else { fallback; } actuate;".asProgram) :: Nil
        case "c" =>
          val (modelplexInput, assumptions) = ModelPlex.createMonitorSpecificationConjecture(modelFml, stateVars.toList.sorted[NamedSymbol]:_*)
          val monitorCond = (monitorKind, ToolProvider.simplifierTool()) match {
            case ("controller", tool) =>
              val foResult = TactixLibrary.proveBy(modelplexInput, ModelPlex.controllerMonitorByChase(1))
              try {
                TactixLibrary.proveBy(foResult.subgoals.head,
                  SaturateTactic(ModelPlex.optimizationOneWithSearch(tool, assumptions)(1)))
              } catch {
                case _: Throwable => foResult
              }
            case ("model", tool) => ??? //@todo sandbox for model monitors
//              TactixLibrary.proveBy(modelplexInput, ModelPlex.modelMonitorByChase(1) &
//              ModelPlex.optimizationOneWithSearch(tool, assumptions)(1) /*& SimplifierV2.simpTac(1)*/)
          }

          if (monitorCond.subgoals.size == 1 && monitorCond.subgoals.head.ante.isEmpty && monitorCond.subgoals.head.succ.size == 1) {
            val monitorFml =  monitorCond.subgoals.head.succ.head
            val reassociatedCtrlMonitorFml = FormulaTools.reassociate(monitorFml)
            val proof = TactixLibrary.proveBy(Equiv(monitorFml, reassociatedCtrlMonitorFml), TactixLibrary.prop)
            if (proof.isProved) {
              val ctrlMonitorProg = TactixLibrary.proveBy(reassociatedCtrlMonitorFml, ModelPlex.chaseToTests(combineTests=false)(1)*2).subgoals.head.succ.head
              val ctrlInputs = CGenerator.getInputs(ctrlMonitorProg)
              val ctrlMonitorCode = (new CGenerator(new CMonitorGenerator()))(ctrlMonitorProg, stateVars, ctrlInputs, "Monitor")
              val inputs = CGenerator.getInputs(prg)
              val fallbackCode = new CControllerGenerator()(prg, stateVars, inputs)
              val declarations = ctrlMonitorCode._1.trim
              val monitorCode = ctrlMonitorCode._2.trim

              val sandbox =
                s"""
                  |${CGenerator.printHeader(model.name)}
                  |$declarations
                  |${fallbackCode._1}
                  |${fallbackCode._2}
                  |$monitorCode
                  |
                  |state ctrl(state curr, const parameters* const params, const input* const in) {
                  |  /* controller implementation stub: modify curr to return actuator set values */
                  |  return curr;
                  |}
                  |
                  |int main() {
                  |  /* control loop stub */
                  |  parameters params; /* set system parameters, e.g., = { .A=1.0 }; */
                  |  while (true) {
                  |    state current; /* read sensor values, e.g., = { .x=0.2 }; */
                  |    input in;   /* resolve non-deterministic assignments in the model */
                  |    state post = monitoredCtrl(current, &params, &in, &ctrl, &ctrlStep);
                  |    /* hand post actuator set values to actuators */
                  |  }
                  |  return 0;
                  |}
                  |""".stripMargin

              new ModelPlexArtifactCodeResponse(model, sandbox) :: Nil
            } else new ErrorResponse("ModelPlex failed: unable to prove equivalence of monitor\n  " + monitorFml.prettyString + " with reassociated form\n  " + reassociatedCtrlMonitorFml.prettyString) :: Nil
          } else new ErrorResponse("ModelPlex failed: expected exactly 1 subgoal, but got " + monitorCond.prettyString) :: Nil
      }
    case _ => new ErrorResponse("Unsupported shape, expected assumptions -> [{ctrl;ode}*]safe, but got " + modelFml.prettyString) :: Nil
  }

  private def createMonitor(model: ModelPOJO, modelFml: Formula, vars: Set[BaseVariable]): List[Response] = {
    val (modelplexInput, assumptions) = ModelPlex.createMonitorSpecificationConjecture(modelFml, vars.toList.sorted[NamedSymbol]:_*)
    val Imply(_, Box(prg, _)) = modelFml
    val monitorCond = (monitorKind, ToolProvider.simplifierTool()) match {
      case ("controller", tool) =>
        val foResult = TactixLibrary.proveBy(modelplexInput, ModelPlex.controllerMonitorByChase(1))
        try {
          TactixLibrary.proveBy(foResult.subgoals.head,
            SaturateTactic(ModelPlex.optimizationOneWithSearch(tool, assumptions)(1)))
        } catch {
          case _: Throwable => foResult
        }
      case ("model", tool) => TactixLibrary.proveBy(modelplexInput, ModelPlex.modelMonitorByChase(1) &
        ModelPlex.optimizationOneWithSearch(tool, assumptions)(1) /*& SimplifierV2.simpTac(1)*/)
    }

    if (monitorCond.subgoals.size == 1 && monitorCond.subgoals.head.ante.isEmpty && monitorCond.subgoals.head.succ.size == 1) {
      val monitorFml =  monitorShape match {
        case "boolean" => monitorCond.subgoals.head.succ.head
        case "metric" => ModelPlex.toMetric(monitorCond.subgoals.head.succ.head)
      }
      conditionKind match {
        case "kym" => new ModelPlexArtifactResponse(model, monitorFml) :: Nil
        case "c" =>
          val inputs = CGenerator.getInputs(prg)
          val monitor = (new CGenerator(new CMonitorGenerator))(monitorFml, vars, inputs, model.name)
          val code =
            s"""
              |${CGenerator.printHeader(model.name)}
              |${monitor._1}
              |${monitor._2}
              |
              |int main() {
              |  /* sandbox stub, select 'sandbox' to auto-generate */
              |  parameters params = { .A=1.0 };
              |  while (true) {
              |    state pre; /* read sensor values, e.g., = { .x=0.0 }; */
              |    state post; /* run controller */
              |    if (!monitorSatisfied(pre,post,params)) post = post; /* replace with fallback control output */
              |    /* hand post actuator set values to actuators */
              |  }
              |  return 0;
              |}
              |""".stripMargin

          new ModelPlexArtifactCodeResponse(model, code) :: Nil
      }
    } else new ErrorResponse("ModelPlex failed: expected exactly 1 subgoal, but got " + monitorCond.prettyString) :: Nil
  }
}

class TestSynthesisRequest(db: DBAbstraction, userId: String, modelId: String, monitorKind: String, testKinds: Map[String, Boolean],
                           amount: Int, timeout: Option[Int]) extends UserRequest(userId) with RegisteredOnlyRequest {
  def resultingResponses(): List[Response]  = {
    logger.debug("Got Test Synthesis Request")
    val model = db.getModel(modelId)
    val modelFml = KeYmaeraXArchiveParser.parseAsProblemOrFormula(model.keyFile)
    val vars = StaticSemantics.boundVars(modelFml).symbols.filter(_.isInstanceOf[BaseVariable]).toList
    val (modelplexInput, assumptions) = ModelPlex.createMonitorSpecificationConjecture(modelFml, vars:_*)
    val monitorCond = (monitorKind, ToolProvider.simplifierTool()) match {
      case ("controller", tool) =>
        val foResult = TactixLibrary.proveBy(modelplexInput, ModelPlex.controllerMonitorByChase(1))
        try {
          TactixLibrary.proveBy(foResult.subgoals.head,
            SaturateTactic(ModelPlex.optimizationOneWithSearch(tool, assumptions)(1)))
        } catch {
          case _: Throwable => foResult
        }
      case ("model", tool) => TactixLibrary.proveBy(modelplexInput,
        ModelPlex.modelMonitorByChase(1) &
        SimplifierV3.simpTac(Nil, SimplifierV3.defaultFaxs, SimplifierV3.arithBaseIndex)(1) &
        ModelPlex.optimizationOneWithSearch(tool, assumptions)(1)
      )
    }

    def variance(vals: Map[Term, Term]): Number = {
      val (pre, post) = vals.partition({ case (v, _) => v.isInstanceOf[BaseVariable] })
      val postByPre: Map[Term, BigDecimal] = post.map({
        case (FuncOf(Function(name, idx, Unit, Real, _), _), Number(value)) if name.endsWith("post") =>
          Variable(name.substring(0, name.length-"post".length), idx) -> value
        case (v, Number(value)) => v -> value
        })
      Number(pre.map({
        case (v, Number(value)) if postByPre.contains(v) => (value - postByPre(v))*(value - postByPre(v))
        case _ => BigDecimal(0)
      }).sum)
    }

    val Imply(True, cond) = monitorCond.subgoals.head.toFormula

    val assumptionsCond = assumptions.reduceOption(And).getOrElse(True)
    val testSpecs: List[(String, Formula)] = testKinds.map({
      case ("compliant", true) => Some("compliant" -> cond)
      case ("incompliant", true) => Some("incompliant" -> Not(cond))
      case _ => None
    }).filter(_.isDefined).map(c => c.get._1 -> And(assumptionsCond, c.get._2)).toList

    val metric = ModelPlex.toMetric(cond)
    ToolProvider.cexTool() match {
      case Some(tool: Mathematica) =>
        val synth = new TestSynthesis(tool)
        //val testCases = synth.synthesizeTestConfig(testCondition, amount, timeout)
        val testCases = testSpecs.map(ts => ts._1 -> synth.synthesizeTestConfig(ts._2, amount, timeout))
        val tcSmVar = testCases.map(tc => tc._1 -> tc._2.map(tcconfig =>
          (tcconfig,
           //@note tcconfig (through findInstance) may contain values that later lead to problems (e.g., division by 0)
           try { Some(synth.synthesizeSafetyMarginCheck(metric, tcconfig)) } catch { case _: ToolException => None },
           variance(tcconfig))
        ))
        new TestSynthesisResponse(model, metric, tcSmVar) :: Nil
      case None => new ErrorResponse("Test case synthesis failed, missing Mathematica") :: Nil
    }
  }
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Proofs of models
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class CreateProofRequest(db : DBAbstraction, userId : String, modelId : String, name : String, description : String)
  extends UserRequest(userId) with WriteRequest {
  def resultingResponses(): List[Response] = {
    val proofId = db.createProofForModel(modelId, name, description, currentDate(), None)
    new CreatedIdResponse(proofId) :: Nil
  }
}

class CreateModelTacticProofRequest(db: DBAbstraction, userId: String, modelId: String) extends UserRequest(userId) with WriteRequest {
  def resultingResponses(): List[Response] = {
    val model = db.getModel(modelId)
    model.tactic match {
      case Some(tacticText) =>
        val proofId = db.createProofForModel(Integer.parseInt(modelId), model.name + " from tactic",
          "Proof from tactic", currentDate(), Some(tacticText))
        new CreatedIdResponse(proofId.toString) :: Nil
      case None => new ErrorResponse("Model " + modelId + " does not have a tactic associated")::Nil
    }
  }
}

class ProofsForModelRequest(db : DBAbstraction, userId: String, modelId: String) extends UserRequest(userId) with ReadRequest {
  def resultingResponses(): List[Response] = {
    val proofs = db.getProofsForModel(modelId).map(proof =>
      (proof, "loaded"/*KeYmaeraInterface.getTaskLoadStatus(proof.proofId.toString).toString.toLowerCase*/))
    new ProofListResponse(proofs) :: Nil
  }
}

class OpenProofRequest(db: DBAbstraction, userId: String, proofId: String, wait: Boolean = false) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val proofInfo = db.getProofInfo(proofId)
    val modelId = proofInfo.modelId
    if (modelId.isEmpty) throw new Exception("Database consistency error: unable to open proof " + proofId + ", because it does not refer to a model")
    else if (db.getModel(modelId.get).userId != userId) new PossibleAttackResponse("Permission denied")::Nil
    else {
      insist(db.getModel(proofInfo.modelId.getOrElse(throw new CoreException(s"Cannot open a proof without model, proofId=$proofId"))).userId == userId, s"User $userId does not own the model associated with proof $proofId")

      proofInfo.modelId match {
        case None => new ErrorResponse("Unable to open proof " + proofId + ", because it does not refer to a model")::Nil // duplicate check to above
        case Some(mId) =>
          val generator = new ConfigurableGenerator[GenProduct]()
          KeYmaeraXParser.setAnnotationListener((p: Program, inv: Formula) =>
            generator.products += (p -> (generator.products.getOrElse(p, Nil) :+ (inv, None))))
          val problem = KeYmaeraXArchiveParser.parseProblem(db.getModel(mId).keyFile)
          session += proofId -> ProofSession(proofId, generator, problem.defs)
          TactixLibrary.invSupplier = generator //@todo should not store invariant generator globally for all users
          new OpenProofResponse(proofInfo, "loaded" /*TaskManagement.TaskLoadStatus.Loaded.toString.toLowerCase()*/) :: Nil
      }
    }
  }
}

class OpenGuestArchiveRequest(db: DBAbstraction, uri: String, archiveName: String) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = {
    try {
      val userId = uri
      val sanitizedUserId = uri.replaceAllLiterally("/", "%2F").replaceAllLiterally(":", "%3A")
      val pwd = "guest"
      val userExists = db.userExists(userId)
      if (!userExists) db.createUser(userId, pwd, "3")

      val models = db.getModelList(userId)
      DatabasePopulator.importKya(db, userId, uri, prove=false, models)

      //@todo template engine, e.g., twirl, or at least figure out how to parse from a string
      val html =
        <html lang="en" ng-app="loginApp" ng-controller="ServerInfoCtrl">
          <head>
            <meta charset="utf-8"/>
            <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
            <meta name="viewport" content="width=device-width, initial-scale=1"/>
            <meta name="description" content=""/>
            <meta name="author" content="Logical Systems Lab, Carnegie Mellon University"/>
            <link rel="icon" href="../../favicon.ico"/>
            <title>KeYmaera X</title>
            <link href="/css/bootstrap.css" rel="stylesheet" type="text/css"/>
            <link href="/css/font-awesome.min.css" rel="stylesheet" type="text/css"/>
            <link href="/css/sticky-footer-navbar.css" rel="stylesheet"/>
          </head>
          <body>
            <script src="/js/jquery.min.js"></script>
            <script src="/js/jquery-ui.min.js"></script>
            <script src="/js/bootstrap/bootstrap.min.js"></script>
            <script src="/js/angular/angular.min.js"></script>
            <script src="/js/angular/angular-sanitize.min.js"></script>
            <script src="/js/angular/angular-cookies.min.js"></script>
            <script src="/js/angular/angular-route.min.js"></script>
            <script src="/js/angular/angular-animate.min.js"></script>
            <script src="/js/angular/bootstrap/ui-bootstrap-tpls-2.5.0.min.js"></script>
            <script src="/js/loginApp.js"></script>
            <script src="/js/services/services.js"></script>
            <script src="/js/services/session.js"></script>
            <script src="/js/controllers/interceptors.js"></script>
            <script src="/js/controllers/auth.js"></script>
            <script src="/js/controllers.js"></script>
            <script src="/js/controllers/factories.js"></script>
            <script src="/js/controllers/errorReport.js"></script>
            <script src="/js/controllers/login.js"></script>
            <script src="/js/controllers/serverinfo.js"></script>

            <div ng-controller="LoginCtrl" ng-init={"login('" + sanitizedUserId + "','" + pwd + "',true);"}></div>
          </body>
        </html>

      HtmlResponse(html) :: Nil
    } catch {
      // Return a user-friendly message, since there's no user interface running yet to render a JSON error response
      case ex: Throwable =>
        val stacktrace = {
          val sw = new StringWriter
          ex.printStackTrace(new PrintWriter(sw))
          sw.toString
        }
        val html =
          <html lang="en" ng-app="loginApp" ng-controller="ServerInfoCtrl">
            <head>
              <meta charset="utf-8"/>
              <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
              <meta name="viewport" content="width=device-width, initial-scale=1"/>
              <meta name="description" content=""/>
              <meta name="author" content="Logical Systems Lab, Carnegie Mellon University"/>
              <link rel="icon" href="../../favicon.ico"/>
              <title>KeYmaera X</title>
              <link href="/css/bootstrap.css" rel="stylesheet" type="text/css"/>
              <link href="/css/font-awesome.min.css" rel="stylesheet" type="text/css"/>
              <link href="/css/sticky-footer-navbar.css" rel="stylesheet"/>
            </head>
            <body>
              <h1>Error browsing archive</h1>
              <p>Please double-check the archive path/name</p>
              <p>Archive location: {archiveName}</p>
              <p>Archive remote location: {uri}</p>
              <h3>Error details</h3>
              <p>{stacktrace}</p>
            </body>
          </html>

        HtmlResponse(html) :: Nil
    }
  }
}

/**
  * Gets all tasks of the specified proof. A task is some work the user has to do. It is not a KeYmaera task!
  *
  * @param db Access to the database.
  * @param userId Identifies the user.
  * @param proofId Identifies the proof.
  */
class GetAgendaAwesomeRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree: ProofTree = DbProofTree(db, proofId)
    val leaves = tree.openGoals
    val closed = tree.openGoals.isEmpty && tree.isProved

    val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList

    // Goals in web UI
    val agendaItems: List[AgendaItem] = leaves.map(n =>
      AgendaItem(n.id.toString, AgendaItem.nameOf(n), proofId))
    // add unexpanded functions, predicates, programs of the open goals of all leaves to the proof session
    val proofSession = session(proofId).asInstanceOf[ProofSession]
    session(proofId) = leaves.flatMap(_.parent).foldLeft(proofSession)(RequestHelper.updateProofSessionDefinitions)
    AgendaAwesomeResponse(tree.info.modelId.get.toString, proofId, tree.root, leaves, agendaItems, closed, marginLeft, marginRight) :: Nil
  }
}

/**
  * Gets the proof root as agenda item (browse a proof from root to leaves).
  * @param db Access to the database.
  * @param userId Identifies the user.
  * @param proofId Identifies the proof.
  */
class GetProofRootAgendaRequest(db: DBAbstraction, userId: String, proofId: String)
    extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree: ProofTree = DbProofTree(db, proofId)
    val agendaItems: List[AgendaItem] = AgendaItem(tree.root.id.toString, AgendaItem.nameOf(tree.root), proofId) :: Nil
    val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
    AgendaAwesomeResponse(tree.info.modelId.get.toString, proofId, tree.root, tree.root::Nil, agendaItems, closed=false, marginLeft, marginRight) :: Nil
  }
}

/**
  * Gets the children of a proof node (browse a proof from root to leaves).
  * @param db Access to the database.
  * @param userId Identifies the user.
  * @param proofId Identifies the proof.
  * @param nodeId Identifies the proof node.
  */
class GetProofNodeChildrenRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String)
    extends UserProofRequest(db, userId, proofId) with ReadRequest {

  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId) match {
      case None => new ErrorResponse("Unknown node " + nodeId) :: Nil
      case Some(node) =>
        val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
        NodeChildrenResponse(proofId, node, marginLeft, marginRight) :: Nil
    }
  }
}

case class GetAgendaItemRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    //@todo seems unused
    ???
//    val closed = db.getProofInfo(proofId).closed
//    val tree = ProofTree.ofTrace(db.getExecutionTrace(proofId.toInt), proofFinished = closed)
//    val possibleItems = db.agendaItemsForProof(proofId.toInt)
//    tree.agendaItemForNode(NodeId.fromString(nodeId), possibleItems) match {
//      case Some(item) => new GetAgendaItemResponse (item) :: Nil
//      case None => new ErrorResponse("No information stored for agenda item " + nodeId) :: Nil
//    }
  }
}

class ProofTaskParentRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId).flatMap(_.parent) match {
      case Some(parent) =>
        val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
        new ProofTaskParentResponse(parent, marginLeft, marginRight)::Nil
      case None => new ErrorResponse("Cannot get parent of node " + nodeId + ", node might be unknown or root")::Nil
    }
  }
}

case class GetPathAllRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.load()
    var node: Option[ProofTreeNode] = tree.locate(nodeId)
    var path: List[ProofTreeNode] = Nil
    while (node.nonEmpty) {
      path = node.get::path
      node = node.get.parent
    }
    val parentsRemaining = 0
    val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
    new GetPathAllResponse(path.reverse, parentsRemaining, marginLeft, marginRight)::Nil
  }
}

case class GetBranchRootRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    var currNode = tree.locate(nodeId)
    var done = false
    while (currNode.flatMap(_.parent).nonEmpty && !done) {
      currNode = currNode.flatMap(_.parent)
      /* Don't stop at the first node just because it branches (it may be the end of one branch and the start of the
      * next), but if we see branching anywhere else we've found the end of our branch. */
      done = currNode.get.children.size > 1
    }
    currNode match {
      case None => new ErrorResponse("Unknown node " + nodeId) :: Nil
      case Some(n) =>
        val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
        new GetBranchRootResponse(n, marginLeft, marginRight) :: Nil
    }

  }
}

class ProofNodeSequentRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId) match {
      case None => throw new Exception("Unknown node " + nodeId)
      case Some(node) =>
        val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
        ProofNodeSequentResponse(proofId, node, marginLeft, marginRight) :: Nil
    }
  }
}

class ProofTaskExpandRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, strict: Boolean)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId) match {
      case None => throw new Exception("Unknown node " + nodeId)
      case Some(node) if node.maker.isEmpty =>
        new ErrorResponse("Unable to expand node " + nodeId + " of proof " + proofId + ", because it did not record a tactic")::Nil
      case Some(node) if node.maker.isDefined =>
        assert(node.maker.isDefined, "Unable to expand node without tactics")
        val (conjecture, parentStep, parentRule) = (ProvableSig.startProof(node.conclusion), node.maker.get, node.makerShortName.get)
        val localProofId = db.createProof(conjecture)
        val innerInterpreter = SpoonFeedingInterpreter(localProofId, -1, db.createProof,
          RequestHelper.listenerFactory(db), ExhaustiveSequentialInterpreter(_, throwWithDebugInfo=false), 1,
          strict=strict, convertPending=false)
        val parentTactic = BelleParser(parentStep)
        innerInterpreter(parentTactic, BelleProvable(conjecture))
        innerInterpreter.kill()

        val trace = db.getExecutionTrace(localProofId)
        val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
        if (trace.steps.size == 1 && trace.steps.head.rule == parentRule) {
          DerivationInfoRegistry.locate(parentTactic) match {
            case Some(ptInfo) => ExpandTacticResponse(localProofId, Nil, Nil,
              ptInfo.codeName, "", Nil, Nil, marginLeft, marginRight) :: Nil
            case None => new ErrorResponse("No further details available") :: Nil
          }
        } else {
          val innerTree = DbProofTree(db, localProofId.toString).load()
          val stepDetails = innerTree.tacticString
          val innerSteps = innerTree.nodes
          val agendaItems: List[AgendaItem] = innerTree.openGoals.map(n =>
            AgendaItem(n.id.toString, AgendaItem.nameOf(n), proofId))
          val goals = innerTree.openGoals.map(_.conclusion)
          val backendGoals = innerTree.openGoals.map(n =>
            if (n.conclusion.isFOL) Some(
              (KeYmaeraToMathematica(n.conclusion.toFormula).toString,
               DefaultSMTConverter(n.conclusion.toFormula)))
            else None
          )

          ExpandTacticResponse(localProofId, goals, backendGoals, parentStep, stepDetails, innerSteps,
            agendaItems, marginLeft, marginRight) :: Nil
        }
    }
  }
}

class StepwiseTraceRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.load()
    val innerSteps = tree.nodes
    val agendaItems: List[AgendaItem] = tree.openGoals.map(n =>
      AgendaItem(n.id.toString, AgendaItem.nameOf(n), proofId.toString))
    //@todo fill in parent step for empty ""
    val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
    ExpandTacticResponse(proofId.toInt, Nil, Nil, "", tree.tacticString, innerSteps, agendaItems, marginLeft, marginRight) :: Nil
  }
}

class GetSequentStepSuggestionRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId) match {
      case None => ApplicableAxiomsResponse(Nil, Map.empty) :: Nil
      case Some(node) => node.goal match {
        case None => ApplicableAxiomsResponse(Nil, Map.empty) :: Nil //@note node closed
        case Some(seq) =>
          if (seq.isFOL) {
            val folSuggestions = "QE"::"abbrv"::"hideL"::Nil
            // todo: counterexample, find assumptions + general help
            val tactics = folSuggestions.map(s => (DerivationInfo(s), None))
            ApplicableAxiomsResponse(tactics, Map.empty) :: Nil
          } else {
            // find "largest" succedent formula with programs and suggest top-level popup content
            val pos = SuccPosition(1)
            ApplicableAxiomsResponse(node.applicableTacticsAt(pos), node.tacticInputSuggestions(pos), Some(Fixed(1))) :: Nil
          }
      }
    }
  }
}

class GetApplicableAxiomsRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, pos: Position)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    if (tree.done) return ApplicableAxiomsResponse(Nil, Map.empty) :: Nil
    tree.locate(nodeId).map(n => (n.applicableTacticsAt(pos), n.tacticInputSuggestions(pos))) match {
      case Some((tactics, inputs)) => ApplicableAxiomsResponse(tactics, inputs) :: Nil
      case None => ApplicableAxiomsResponse(Nil, Map.empty) :: Nil
    }
  }
}

class GetApplicableTwoPosTacticsRequest(db:DBAbstraction, userId: String, proofId: String, nodeId: String,
                                        pos1: Position, pos2: Position) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    if (tree.done) return new ApplicableAxiomsResponse(Nil, Map.empty) :: Nil
    tree.locate(nodeId).map(n => n.applicableTacticsAt(pos1, Some(pos2))) match {
      case None => new ApplicableAxiomsResponse(Nil, Map.empty) :: Nil
      case Some(tactics) => new ApplicableAxiomsResponse(tactics, Map.empty) :: Nil
    }
  }
}

class GetDerivationInfoRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String,
                               axiomId: Option[String]) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val infos = axiomId match {
      case Some(aid) => (DerivationInfo.ofCodeName(aid), UIIndex.comfortOf(aid).map(DerivationInfo.ofCodeName)) :: Nil
      case None => DerivationInfo.allInfo.
        filter(di => di.displayLevel != 'internal).
        map(di => (di, UIIndex.comfortOf(di.codeName).map(DerivationInfo.ofCodeName)))
    }
    ApplicableAxiomsResponse(infos, Map.empty) :: Nil
  }
}

/** Gets the definitions that can be expanded at node `nodeId`. */
class GetApplicableDefinitionsRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    if (tree.done) return ApplicableDefinitionsResponse(Nil) :: Nil
    val proofSession = session(proofId).asInstanceOf[ProofSession]
    tree.locate(nodeId).map(n => n.goal.map(StaticSemantics.symbols).getOrElse(Set.empty)) match {
      case Some(symbols) =>
        //@todo InputSignature no longer available from simplified parser -> simplify data structure
        val applicable: Map[NamedSymbol, (Signature, Option[InputSignature])] = symbols.
          filter({ case _: Function => true case _: ProgramConst => true case _ => false }).
          flatMap(s => {
            val defs = proofSession.defs.find(s.name, s.index)
            defs match {
              case Some(f) => Some(s -> (f, None))
              case None => None
            }
          }).toMap
        // name, name expression (what), optional repl, optional plaintext definition from the model file
        val expansions: List[(NamedSymbol, Expression, Option[Expression], Option[InputSignature])] = applicable.toList.map({
          // functions, predicates, and programs with definition
          case (s: Function, ((domain, sort, _, repl, _), insig)) if repl.isDefined =>
            val arg = insig.map(_._1.map(_.asInstanceOf[Variable]).reduceRightOption(Pair).getOrElse(Nothing)).getOrElse(domain.getOrElse(Unit).toDots(0)._1)
            sort match {
              case Real => (s, FuncOf(s, arg), repl, insig)
              case Bool => (s, PredOf(s, arg), repl, insig)
            }
          case (s: ProgramConst, ((_, _, _, repl, _), insig)) if repl.isDefined => (s, s, repl, insig)
          // functions, predicates, and programs without definition
          case (s: Function, ((domain, sort, _, None, _), _)) =>
            val arg = domain.map({ case edu.cmu.cs.ls.keymaerax.core.Unit => Nothing case d => d.toDots(0)._1}).getOrElse(Nothing)
            sort match {
              case Real => (s, FuncOf(s, arg), None, None)
              case Bool => (s, PredOf(s, arg), None, None)
            }
          case (s: ProgramConst, ((_, _, _, None, _), _)) => (s, s, None, None)
        })
        ApplicableDefinitionsResponse(expansions.sortBy(_._1)) :: Nil
      case None => ApplicableDefinitionsResponse(Nil) :: Nil
    }
  }
}

class SetDefinitionsRequest(db: DBAbstraction, userId: String, proofId: String, what: String, repl: String)
  extends UserProofRequest(db, userId, proofId) with WriteRequest {
  override protected def doResultingResponses(): List[Response] = {
    val proofSession = session(proofId).asInstanceOf[ProofSession]
    Try(what.asExpr).toEither match {
      case Left(ex) => BooleanResponse(flag = false, Some("Unable to parse 'what': " + ex.getMessage)) :: Nil
      case Right(e) =>
        val (name: String, index: Option[Int], domain: Option[Sort], sort: Sort) = e match {
          case FuncOf(Function(n, i, d, s, _), _) =>
            // uninterpreted predicates parse as functions when parsed standalone
            proofSession.defs.asNamedSymbols.find(ns => ns.name == n && ns.index == i) match {
              case Some(ns: Function) if ns.domain == d => (n, i, Some(d), ns.sort)
              case None => (n, i, Some(d), s)
            }
          case PredOf(Function(n, i, d, s, _), _) => (n, i, Some(d), s)
          case ProgramConst(n, _) => (n, None, None, Trafo)
        }

        Try(repl.asExpr).toEither match {
          case Left(ex) => BooleanResponse(flag = false, Some("Unable to parse 'repl': " + ex.getMessage)) :: Nil
          case Right(r) if r.sort == sort =>
            session(proofId) = proofSession.copy(defs = proofSession.defs.copy(decls = proofSession.defs.decls +
              ((name, index) -> (domain, sort, None, Some(r), UnknownLocation))))
            BooleanResponse(flag = true) :: Nil
          case Right(r) if r.sort != sort =>
            BooleanResponse(flag = false, Some("Expected a replacement of sort " + sort + ", but got " + r.sort)) :: Nil
        }
    }
  }
}

class ExportCurrentSubgoal(db: DBAbstraction, userId: String, proofId: String, nodeId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    DbProofTree(db, proofId).locate(nodeId).flatMap(_.goal) match {
      case None => new ErrorResponse("Unknown node " + nodeId) :: Nil
      case Some(goal) =>
        val provable = ProvableSig.startProof(goal)
        val lemma = Lemma.apply(provable, List(ToolEvidence(List("tool" -> "mock"))), None)
        new KvpResponse("sequent", "Sequent: \n" + goal.toString + "\n\nProvable: \n" + provable.prettyString + "\n\nLemma:\n" + lemma.toString) :: Nil
    }
  }
}

case class BelleTermInput(value: String, spec:Option[ArgInfo])

class GetStepRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, pos: Position)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.locate(nodeId).flatMap(_.goal) match {
      case None => new ApplicableAxiomsResponse(Nil, Map.empty) :: Nil
      case Some(goal) =>
        goal.sub(pos) match {
          case Some(fml: Formula) =>
            UIIndex.theStepAt(fml, Some(pos)) match {
              case Some(step) => new ApplicableAxiomsResponse((DerivationInfo(step), None) :: Nil, Map.empty) :: Nil
              case None => new ApplicableAxiomsResponse(Nil, Map.empty) :: Nil
            }
          case _ => new ApplicableAxiomsResponse(Nil, Map.empty) :: Nil
        }
    }
  }
}

class GetLemmasRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, pos: Position,
                        partialLemmaName: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val infos = ProvableInfo.allInfo.filter(i =>
      (i.isInstanceOf[CoreAxiomInfo] || i.isInstanceOf[DerivedAxiomInfo]) && i.canonicalName.contains(partialLemmaName))
    LemmasResponse(infos)::Nil
  }
}

class GetFormulaPrettyStringRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, pos: Position)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    DbProofTree(db, proofId).locate(nodeId).flatMap(_.goal.flatMap(_.sub(pos))) match {
      case None => new ErrorResponse("Unknown position " + pos + " at node " + nodeId)::Nil
      case Some(e: Expression) => new PlainResponse("prettyString" -> JsString(e.prettyString))::Nil
    }
  }
}

class CheckTacticInputRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, tacticId: String,
                              paramName: String, paramType: String, paramValue: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {

  /** Prints a sort as users might expect from other web UI presentations. */
  private def printSort(s: Sort): String = s match {
    case Unit => ""
    case Real => "Real"
    case Bool => "Bool"
    case Tuple(l, r) => printSort(l) + "," + printSort(r)
  }

  /** Prints a named symbol as users might expect from other web UI presentations. */
  private def printNamedSymbol(n: NamedSymbol): String = n match {
    case _: Variable => "variable " + n.prettyString
    case Function(_, _, domain, _, _) => "function " + n.prettyString + "(" + printSort(domain) + ")"
  }

  /** Basic input sanity checks w.r.t. symbols in `sequent`. */
  private def checkInput(sequent: Sequent, input: BelleTermInput, defs: KeYmaeraXArchiveParser.Declaration): Response = {
    try {
      input match {
        case BelleTermInput(value, Some(arg: TermArg)) => checkExpressionInput(arg, value.asExpr :: Nil, sequent, defs)
        case BelleTermInput(value, Some(arg: FormulaArg)) => checkExpressionInput(arg, value.asExpr :: Nil, sequent, defs)
        case BelleTermInput(value, Some(arg: VariableArg)) => checkExpressionInput(arg, value.asExpr :: Nil, sequent, defs)
        case BelleTermInput(value, Some(arg: ExpressionArg)) => checkExpressionInput(arg, value.asExpr :: Nil, sequent, defs)
        case BelleTermInput(value, Some(arg: SubstitutionArg)) => checkSubstitutionInput(arg, value.asSubstitutionPair :: Nil, sequent, defs)
        case BelleTermInput(value, Some(OptionArg(arg))) if !arg.isInstanceOf[SubstitutionArg] => checkExpressionInput(arg, value.asExpr :: Nil, sequent, defs)
        case BelleTermInput(value, Some(OptionArg(arg))) if  arg.isInstanceOf[SubstitutionArg] =>
          checkSubstitutionInput(arg, value.asSubstitutionPair :: Nil, sequent, defs)
        case BelleTermInput(value, Some(arg@ListArg(ai: FormulaArg))) => checkExpressionInput(arg, value.split(",").map(KeYmaeraXParser).toList, sequent, defs)
      }
    } catch {
      case ex: ParseException => BooleanResponse(flag=false, Some(ex.toString))
    }
  }

  /** Checks expression inputs. */
  private def checkExpressionInput[E <: Expression](arg: ArgInfo, exprs: List[E], sequent: Sequent,
                                                    defs: KeYmaeraXArchiveParser.Declaration) = {
    val sortMismatch: Option[String] = (arg, exprs) match {
      case (_: VariableArg, (v: Variable) :: Nil) => DerivationInfoRegistry.convert(arg, List(v)).right.toOption
      case (_: TermArg, (t: Term) :: Nil) => DerivationInfoRegistry.convert(arg, List(t)).right.toOption
      case (_: FormulaArg, (f: Formula) :: Nil) => DerivationInfoRegistry.convert(arg, List(f)).right.toOption
      case (_: ExpressionArg, (e: Expression) :: Nil) => DerivationInfoRegistry.convert(arg, List(e)).right.toOption
      case (ListArg(ai: FormulaArg), fmls) if fmls.forall(_.kind == FormulaKind) => None
      case _ => Some("Expected: " + arg.sort + ", found: " + exprs.map(_.kind).mkString(",") + " " +   exprs.map(_.prettyString).mkString(","))
    }

    sortMismatch match {
      case None =>
        val symbols = StaticSemantics.symbols(sequent) ++ defs.asNamedSymbols + Function("old", None, Real, Real)
        val paramFV: Set[NamedSymbol] =
          exprs.flatMap(e => StaticSemantics.freeVars(e).toSet ++ StaticSemantics.signature(e)).toSet

        val (hintFresh, allowedFresh) = arg match {
          case _: VariableArg if arg.allowsFresh.contains(arg.name) => (Nil, Nil)
          case _ => (paramFV -- symbols, arg.allowsFresh) //@todo would need other inputs to check
        }

        if (hintFresh.size > allowedFresh.size) {
          val fnVarMismatch = hintFresh.map(fn => fn -> symbols.find(s => s.name == fn.name && s.index == fn.index)).
            filter(_._2.isDefined)
          if (fnVarMismatch.isEmpty) {
            BooleanResponse(flag = false, Some("Argument " + arg.name + " uses new names that do not occur in the sequent: " + hintFresh.mkString(",") +
              (if (allowedFresh.nonEmpty) ", expected new names only as introduced for " + allowedFresh.mkString(",")
              else ", is it a typo?")))
          } else BooleanResponse(flag=true)
        } else {
          BooleanResponse(flag=true)
        }
      case Some(mismatch) => BooleanResponse(flag=false, Some(mismatch))
    }
  }

  /** Checks substitution inputs. */
  private def checkSubstitutionInput(arg: ArgInfo, exprs: List[SubstitutionPair], sequent: Sequent,
                                     defs: KeYmaeraXArchiveParser.Declaration) = {
    //@note parsed as substitution pair is all we check for now
    BooleanResponse(flag=true)
  }

  override protected def doResultingResponses(): List[Response] = {
    val info = DerivationInfo(tacticId)
    val expectedInputs = info.inputs
    val paramInfo = expectedInputs.find(_.name == paramName)
    val isIllFormed = paramInfo.isEmpty || paramValue.isEmpty
    if (!isIllFormed) {
      val input = BelleTermInput(paramValue, paramInfo)

      val tree: ProofTree = DbProofTree(db, proofId)
      tree.locate(nodeId) match {
        case None => BooleanResponse(flag=false, Some("Unknown node " + nodeId + " in proof " + proofId)) :: Nil
        case Some(node) if node.goal.isEmpty => BooleanResponse(flag=false, Some("Node " + nodeId + " does not have a goal")) :: Nil
        case Some(node) if node.goal.isDefined =>
          val sequent = node.goal.get
          val proofSession = session(proofId).asInstanceOf[ProofSession]
          checkInput(sequent, input, proofSession.defs)::Nil
      }
    } else {
      val msg =
        if (paramValue.isEmpty) "Missing value of parameter " + paramName
        else "Parameter " + paramName + " not a valid argument of tactic " + tacticId + ", expected one of " + expectedInputs.map(_.name).mkString(",")
      BooleanResponse(flag=false, Some(msg))::Nil
    }
  }

}

/* If pos is Some then belleTerm must parse to a PositionTactic, else if pos is None belleTerm must parse
* to a Tactic */
class RunBelleTermRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, belleTerm: String,
                          pos: Option[PositionLocator], pos2: Option[PositionLocator] = None,
                          inputs:List[BelleTermInput] = Nil, consultAxiomInfo: Boolean = true, stepwise: Boolean = false)
  extends UserProofRequest(db, userId, proofId) with WriteRequest {
  /** Turns belleTerm into a specific tactic expression, including input arguments */
  private def fullExpr(sequent: Sequent): String = {
    val paramStrings: List[String] = inputs.map{
      case BelleTermInput(value, Some(_:TermArg)) => "{`"+value+"`}"
      case BelleTermInput(value, Some(_:FormulaArg)) => "{`"+value+"`}"
      case BelleTermInput(value, Some(_:VariableArg)) => "{`"+value+"`}"
      case BelleTermInput(value, Some(_:ExpressionArg)) => "{`"+value+"`}"
      case BelleTermInput(value, Some(_:SubstitutionArg)) => "{`"+value+"`}"
      case BelleTermInput(value, Some(ListArg(ai: FormulaArg))) => "[" + value.split(",").map("{`"+_+"`}").mkString(",") + "]"
      case BelleTermInput(value, Some(_:StringArg)) => "{`"+value+"`}"
      case BelleTermInput(value, Some(OptionArg(_: ListArg))) => "[" + value.split(",").map("{`"+_+"`}").mkString(",") + "]"
      case BelleTermInput(value, Some(OptionArg(_))) => "{`"+value+"`}"
      case BelleTermInput(value, None) => value
    }
    //@note stepAt(pos) may refer to a search tactic without position (e.g, closeTrue, closeFalse)
    val (specificTerm: String, adaptedPos: Option[PositionLocator], adaptedPos2: Option[PositionLocator]) =
      if (consultAxiomInfo) RequestHelper.getSpecificName(belleTerm, sequent, pos, pos2) match {
        case Left(s) => (s, pos, pos2)
        //@note improve tactic maintainability by position search with formula shape on universally applicable tactics
        case Right(t: PositionTacticInfo) if t.codeName == "hideL" || t.codeName == "hideR" => pos match {
          case Some(Fixed(pp, None, _)) if pp.isTopLevel => (t.codeName, Some(Fixed(pp, Some(sequent(pp.top)))), None)
          case _ => (t.codeName, pos, None)
        }
        case Right(t: PositionTacticInfo) => (t.codeName, pos, None)
        case Right(t: InputPositionTacticInfo) => (t.codeName, pos, None)
        case Right(t: TwoPositionTacticInfo) => (t.codeName, pos, pos2)
        case Right(t: InputTwoPositionTacticInfo) => (t.codeName, pos, pos2)
        case Right(t: BuiltinInfo) => (t.codeName, None, None)
        case Right(t) => (t.codeName, None, None)
      }
      else (belleTerm, pos, pos2)

    if (inputs.isEmpty && adaptedPos.isEmpty) { assert(adaptedPos2.isEmpty, "Undefined pos1, but defined pos2"); specificTerm }
    else if (inputs.isEmpty && adaptedPos.isDefined && adaptedPos2.isEmpty) { specificTerm + "(" + adaptedPos.get.prettyString + ")" }
    else if (inputs.isEmpty && adaptedPos.isDefined && adaptedPos2.isDefined) { specificTerm + "(" + adaptedPos.get.prettyString + "," + adaptedPos2.get.prettyString + ")" }
    else specificTerm + "(" + paramStrings.mkString(",") + ")"
  }

  private class TacticPositionError(val msg:String,val pos: edu.cmu.cs.ls.keymaerax.parser.Location,val inlineMsg: String) extends Exception

  private def backendAvailable: Boolean = Configuration(Configuration.Keys.QE_TOOL) match {
    case "mathematica" => ToolProvider.tool("Mathematica") match {
      case Some(mathematica: Mathematica) => mathematica.getAvailableWorkers > 0
      case None => ToolProvider.qeTool() match {
        case Some(t: ToolOperationManagement) => t.getAvailableWorkers > 0
        case _ => false
      }
      case _ => false
    }
    case "wolframengine" => ToolProvider.tool("WolframEngine") match {
      case Some(mathematica: Mathematica) => mathematica.getAvailableWorkers > 0
      case None => ToolProvider.qeTool() match {
        case Some(t: ToolOperationManagement) => t.getAvailableWorkers > 0
        case _ => false
      }
      case _ => false
    }
    case "wolframscript" => ToolProvider.tool("WolframScript") match {
      case Some(mathematica: Mathematica) => mathematica.getAvailableWorkers > 0
      case None => ToolProvider.qeTool() match {
        case Some(t: ToolOperationManagement) => t.getAvailableWorkers > 0
        case _ => false
      }
      case _ => false
    }
    case "z3" => ToolProvider.tool("Z3") match {
      case Some(z3: Z3) => z3.getAvailableWorkers > 0
      case _ => false
    }
  }

  private def executionInfo(ruleName: String): String = ruleName + ": " + (ruleName match {
    case "solve" | "ODE" =>
      """
        |If it takes too long: provide invariants of the ODE using dC, and prove the invariants with ODE or
        |if necessary one of the specialized ODE proof tactics, such as dI.
      """.stripMargin
    case "QE" =>
      """
        |If it takes too long, try to simplify arithmetic:
        |(1) hide irrelevant assumptions
        |(2) split into multiple goals
        |(3) expand and simplify special functions
        |(4) abbreviate or simplify complicated terms
      """.stripMargin
    case _ => ""
  })

  override protected def doResultingResponses(): List[Response] = {
    if (backendAvailable) {
      val proof = db.getProofInfo(proofId)
      if (proof.closed) new ErrorResponse("Can't execute tactics on a closed proof") :: Nil
      else {
        val tree: ProofTree = DbProofTree(db, proofId)
        tree.locate(nodeId) match {
          case None => new ErrorResponse("Unknown node " + nodeId + " in proof " + proofId) :: Nil
          case Some(node) if node.goal.isEmpty => new ErrorResponse("Node " + nodeId + " does not have a goal") :: Nil
          case Some(node) if node.goal.isDefined =>
            val sequent = node.goal.get

            try {
              val proofSession = session(proofId).asInstanceOf[ProofSession]
              val tacticString = fullExpr(sequent)
              // elaborate all variables to function/predicate symbols, but never auto-expand
              val expr = BelleParser.parseWithInvGen(tacticString, Some(proofSession.invGenerator), proofSession.defs, expandAll=false)

              val appliedExpr: BelleExpr = (pos, pos2, expr) match {
                case (None, None, _: AtPosition[BelleExpr]) =>
                  throw new TacticPositionError("Can't run a positional tactic without specifying a position", expr.getLocation, "Expected position in argument list but found none")
                case (None, None, _) => expr
                case (Some(position), None, expr: AtPosition[BelleExpr]) => expr(position)
                case (Some(_), None, expr: BelleExpr) => expr
                case (Some(Fixed(p1, None, _)), Some(Fixed(p2, None, _)), expr: BuiltInTwoPositionTactic) => expr(p1, p2)
                case (Some(_), Some(_), expr: BelleExpr) => expr
                case _ => logger.error("Position error running tactic at pos " + pos.getClass.getName + ", expr " + expr.getClass.getName); throw new ProverException("Match error")
              }

              val ruleName =
                if (consultAxiomInfo) RequestHelper.getSpecificName(belleTerm, sequent, pos, pos2, _ => appliedExpr.prettyString)
                else "custom"

              def interpreter(proofId: Int, startNodeId: Int) = new Interpreter {
                val inner = SpoonFeedingInterpreter(proofId, startNodeId, db.createProof, RequestHelper.listenerFactory(db),
                  ExhaustiveSequentialInterpreter(_, throwWithDebugInfo = false), 0, strict = false)

                override def apply(expr: BelleExpr, v: BelleValue): BelleValue = try {
                  inner(expr, v)
                } catch {
                  case ex: Throwable => inner.innerProofId match {
                    case Some(innerId) =>
                      //@note display progress of inner (Let) proof, works only in stepwise execution (step details dialog)
                      val innerTrace = db.getExecutionTrace(innerId)
                      if (innerTrace.steps.nonEmpty) BelleSubProof(innerId)
                      else throw new BelleNoProgress("No progress", ex)
                    case None => throw ex
                  }
                }

                override def kill(): Unit = inner.kill()

                override def isDead: Boolean = inner.isDead

                override def listeners: Seq[IOListener] = inner.listeners
              }

              if (stepwise) {
                if (ruleName == "custom") {
                  //@note execute tactic scripts step-by-step for better browsing
                  val startStepIndex = node.id match {
                    case DbStepPathNodeId(id, _) => db.getExecutionSteps(proofId.toInt).indexWhere(_.stepId == id)
                    case _ => throw new Exception("Unexpected node ID shape " + node.id.toString
                      + ". Expected step path ID of the form (node ID,branch index)")
                  }
                  val taskId = node.stepTactic(userId, interpreter(proofId.toInt, startStepIndex), appliedExpr)
                  RunBelleTermResponse(proofId, node.id.toString, taskId, "Executing custom tactic") :: Nil
                } else {
                  val localProvable = ProvableSig.startProof(sequent)
                  val localProofId = db.createProof(localProvable)
                  val executor = BellerophonTacticExecutor.defaultExecutor
                  val taskId = executor.schedule(userId, appliedExpr, BelleProvable(localProvable, node.label.map(_ :: Nil)), interpreter(localProofId, -1))
                  RunBelleTermResponse(localProofId.toString, "()", taskId, "Executing internal steps of " + executionInfo(belleTerm)) :: Nil
                }
              } else {
                //@note execute clicked single-step tactics on sequential interpreter right away
                val taskId = node.runTactic(userId, ExhaustiveSequentialInterpreter(_, throwWithDebugInfo = false), appliedExpr, ruleName)
                val info = "Executing " + executionInfo(belleTerm)
                RunBelleTermResponse(proofId, node.id.toString, taskId, info) :: Nil
              }
            } catch {
              case e: ProverException if e.getMessage == "No step possible" => new ErrorResponse("No step possible") :: Nil
              case e: TacticPositionError => new TacticErrorResponse(e.msg, HackyInlineErrorMsgPrinter(belleTerm, e.pos, e.inlineMsg), e) :: Nil
              case e: BelleThrowable => new TacticErrorResponse(e.getMessage, HackyInlineErrorMsgPrinter(belleTerm, UnknownLocation, e.getMessage), e) :: Nil
            }
        }
      }
    } else {
      new ErrorResponse("Backend tool unavailable or busy. If the tool remains unavailable, please restart KeYmaera X and/or configure a different tool") :: Nil
    }
  }
}

/** Create a proof if it does not exist yet. Read request, so that guest users can check proofs. */
class InitializeProofFromTacticRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val proofInfo = db.getProofInfo(proofId)
    proofInfo.tactic match {
      case None => new ErrorResponse("Proof " + proofId + " does not have a tactic") :: Nil
      case Some(_) if proofInfo.modelId.isEmpty => throw new Exception("Proof " + proofId + " does not refer to a model")
      case Some(t) if proofInfo.modelId.isDefined =>
        val proofSession = session(proofId).asInstanceOf[ProofSession]
        //@note do not auto-expand if tactic contains verbatim expands or "pretty-printed" expands (US)
        val tactic =
          if ("(expand(?!All))|(expandAllDefs)".r.findFirstIn(t).isDefined) BelleParser.parseWithInvGen(t, None, proofSession.defs)
          else if ("""US\([^)]*\)""".r.findFirstIn(t).isDefined) BelleParser.parseWithInvGen(t, None, proofSession.defs)
          else BelleParser.parseWithInvGen(t, None, proofSession.defs, expandAll = true) // backwards compatibility

        def atomic(name: String): String = {
          val tree: ProofTree = DbProofTree(db, proofId)
          tree.root.runTactic(userId, ExhaustiveSequentialInterpreter(_, throwWithDebugInfo = false), tactic, name)
        }

        tactic match {
          case n: NamedBelleExpr => RunBelleTermResponse(proofId, "()", atomic(n.name), "") :: Nil
          case _ =>
            try {
              //@note replace listener created by proof tree (we want a different tactic name for each component of the
              // executed tactic and we want to see progress)
              val interpreter = (_: List[IOListener]) => DatabasePopulator.prepareInterpreter(db, proofId.toInt,
                CollectProgressListener() :: Nil)
              val tree: ProofTree = DbProofTree(db, proofId)
              val executor = BellerophonTacticExecutor.defaultExecutor
              val taskId = tree.root.runTactic(userId, interpreter, tactic, "", executor)
              RunBelleTermResponse(proofId, "()", taskId, "") :: Nil
            } catch {
              case _: Throwable =>
                //@note if spoonfeeding interpreter fails, try sequential interpreter so that tactics at least proofcheck
                //      even if browsing then shows a single step only
                RunBelleTermResponse(proofId, "()", atomic("custom"), "") :: Nil
            }
        }
    }
  }
}

class TaskStatusRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, taskId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val executor = BellerophonTacticExecutor.defaultExecutor
    type Progress = (Option[(BelleExpr, Long)], Seq[(BelleExpr, Either[BelleValue, BelleThrowable])])
    val (isDone, progress: Option[Progress]) = executor.synchronized {
      executor.getTask(taskId) match {
        case Some(task) =>
          val progressList = task.interpreter match {
            case SpoonFeedingInterpreter(_, _, _, _, interpreterFactory, _, _, _) =>
              //@note the inner interpreters have CollectProgressListeners attached
              interpreterFactory(Nil).listeners.flatMap({
                case l@CollectProgressListener(p) => Some(
                  l.getCurrentTactic.map(f => (f._1, System.currentTimeMillis() - f._2)),
                  scala.collection.immutable.Seq(p:_*))
                case _ => None
              }).headOption
            case _ => None
          }
          (executor.isDone(taskId), progressList)
        case _ => (!executor.contains(taskId) || executor.isDone(taskId), None)
      }
    }
    TaskStatusResponse(proofId, nodeId, taskId, if (isDone) "done" else "running", progress) :: Nil
  }
}

class TaskResultRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, taskId: String)
  extends UserProofRequest(db, userId, proofId) with ReadRequest {
  /* It's very important not to report a branch as closed when it isn't. Other wise the user will carry on in blissful
  * ignorance thinking the hardest part of their proof is over when it's not. This is actually a bit difficult to get
  * right, so check the actual provables to make sure we're closing a branch. */
  private def noBogusClosing(tree: ProofTree, pn: ProofTreeNode): Boolean =
    pn.children.size == pn.localProvable.subgoals.size &&
      pn.children.zip(pn.localProvable.subgoals).forall({case (c, sg) => c.localProvable.conclusion == sg})

  override protected def doResultingResponses(): List[Response] = {
    val executor = BellerophonTacticExecutor.defaultExecutor
    val marginLeft::marginRight::Nil = db.getConfiguration(userId).config.getOrElse("renderMargins", "[40,80]").parseJson.convertTo[Array[Int]].toList
    executor.synchronized {
      val response = executor.wait(taskId) match {
        case Some(Left(BelleProvable(_, _))) =>
          val tree = DbProofTree(db, proofId)
          tree.locate(nodeId) match {
            case None => new ErrorResponse("Unknown node " + nodeId)
            case Some(node) =>
              //@todo construct provable (expensive!)
              //assert(noBogusClosing(tree, node), "Server thinks a goal has been closed when it clearly has not")
              val proofSession = session(proofId).asInstanceOf[ProofSession]
              session(proofId) = RequestHelper.updateProofSessionDefinitions(proofSession, node)
              TaskResultResponse(proofId, node, marginLeft, marginRight, progress=true)
          }
//          val positionLocator = if (parentNode.children.isEmpty) None else RequestHelper.stepPosition(db, parentNode.children.head)
//          assert(noBogusClosing(finalTree, parentNode), "Server thinks a goal has been closed when it clearly has not")
//          new TaskResultResponse(proofId, parentNode, positionLocator, progress = true)
        case Some(Left(BelleSubProof(subId))) =>
          //@todo untested with new tree data structure
          //@HACK for stepping into Let steps
          val tree = DbProofTree(db, subId.toString)
          val node = tree.root//findNode(nodeId).get
          //val positionLocator = if (parentNode.subgoals.isEmpty) None else RequestHelper.stepPosition(db, parentNode.children.head)
          assert(noBogusClosing(tree, node), "Server thinks a goal has been closed when it clearly has not")
          TaskResultResponse(subId.toString, node, marginLeft, marginRight, progress = true)
        case Some(Right(error: BelleThrowable)) => new TacticErrorResponse(error.getMessage, "", error)
        case None => new ErrorResponse("Tactic cancelled, proof state may not reflect result of full tactic")
      }
      //@note may have been cancelled in the meantime
      executor.tryRemove(taskId)
      response :: Nil
    }
  }
}

class StopTaskRequest(db: DBAbstraction, userId: String, proofId: String, nodeId: String, taskId: String)
  extends UserProofRequest(db, userId, proofId) with WriteRequest {
  override protected def doResultingResponses(): List[Response] = {
    val executor = BellerophonTacticExecutor.defaultExecutor
    //@note may have completed in the meantime
    executor.tasksForUser(userId).foreach(executor.tryRemove(_, force = true))
    new GenericOKResponse() :: Nil
  }
}

/** Prunes a node and everything below */
class PruneBelowRequest(db : DBAbstraction, userId : String, proofId : String, nodeId : String) extends UserProofRequest(db, userId, proofId) with WriteRequest {
  override protected def doResultingResponses(): List[Response] = {
    if (db.getProofInfo(proofId).closed) new ErrorResponse("Pruning not allowed on closed proofs") :: Nil
    else {
      val tree = DbProofTree(db, proofId)
      tree.locate(nodeId) match {
        case None => new ErrorResponse("Unknown node " + nodeId) :: Nil
        case Some(node) =>
          node.pruneBelow()
          val item = AgendaItem(node.id.toString, AgendaItem.nameOf(node), proofId)
          new PruneBelowResponse(item) :: Nil
      }
    }
  }
}

/** Undoes the last proof step. */
class UndoLastProofStepRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with WriteRequest {
  private def agendaItemName(codeName: String): String = {
    Try(DerivationInfo.ofCodeName(codeName)).toOption match {
      case Some(di) => di.display.name
      case None => codeName
    }
  }

  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    //@todo do not load all steps
    tree.nodes.lastOption.flatMap(_.parent) match {
      case None => new ErrorResponse("Proof does not have any steps yet") :: Nil
      case Some(node) =>
        node.pruneBelow()
        val info = db.getProofInfo(proofId)
        db.updateProofInfo(info.copy(closed = false))
        val item = AgendaItem(node.id.toString,
          AgendaItem.nameOf(node)
          ,
          proofId, node.allAncestors.map(_.id.toString))
        new PruneBelowResponse(item) :: Nil
    }
  }
}

class GetProofProgressStatusRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    // @todo return Loading/NotLoaded when appropriate
    val proof = db.getProofInfo(proofId)
    new ProofProgressResponse(proofId, isClosed = proof.closed) :: Nil
  }
}

class CheckIsProvedRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  private def exportLemma(lemmaName: String, model: ModelPOJO, provable: ProvableSig, tactic: String) = {
    if (!LemmaDBFactory.lemmaDB.contains(lemmaName)) {
      val evidence = Lemma.requiredEvidence(provable, ToolEvidence(List(
        "tool" -> "KeYmaera X",
        "model" -> model.keyFile,
        "tactic" -> tactic
      )) :: Nil)
      LemmaDBFactory.lemmaDB.add(new Lemma(provable, evidence, Some(lemmaName)))
    }
  }
  private def backupProof(model: ModelPOJO, provable: ProvableSig, tactic: String) = {
    val proofbackupPath = Paths.get(Configuration.KEYMAERAX_HOME_PATH + File.separator + "proofbackup")
    if (!Files.exists(proofbackupPath)) Files.createDirectories(proofbackupPath)

    val sanitizedModelName = model.name.replaceAll("\\W", "_")
    val proofName = sanitizedModelName + "_" + new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(Calendar.getInstance().getTime)
    var i = 0
    var uniqueProofName = proofName
    while (Files.exists(proofbackupPath.resolve(uniqueProofName))) {
      i = i+1
      uniqueProofName = proofName + "_" + i
    }

    val archiveContent = ArchiveEntryPrinter.archiveEntry(model, (proofName, tactic)::Nil, withComments=false)
    Files.write(proofbackupPath.resolve(uniqueProofName), archiveContent.getBytes())
  }

  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.load()
    val model = db.getModel(tree.info.modelId.get)
    val entry = KeYmaeraXArchiveParser.parse(model.keyFile, parseTactics=false).head
    val conclusionFormula = entry.defs.exhaustiveSubst[Formula](entry.model.asInstanceOf[Formula])
    val conclusion = Sequent(IndexedSeq(), IndexedSeq(conclusionFormula))
    val provable = tree.root.provable
    if (!provable.isProved) new ErrorResponse("Proof verification failed: proof " + proofId + " is not closed.\n Expected a provable without subgoals, but result provable is\n" + provable.prettyString)::Nil
    else if (provable.conclusion != conclusion) new ErrorResponse("Proof verification failed: proof " + proofId + " does not conclude the associated model.\n Expected " + conclusion.prettyString + "\nBut got\n" + provable.conclusion.prettyString)::Nil
    else {
      assert(provable.isProved, "Provable " + provable + " must be proved")
      assert(provable.conclusion == conclusion, "Conclusion of provable " + provable + " must match problem " + conclusion)
      val tactic = tree.tacticString
      // remember tactic string
      val newInfo = ProofPOJO(tree.info.proofId, tree.info.modelId, tree.info.name, tree.info.description,
        tree.info.date, tree.info.stepCount, tree.info.closed, tree.info.provableId, tree.info.temporary,
        Some(tactic))
      db.updateProofInfo(newInfo)
      // remember lemma
      exportLemma("user" + File.separator + model.name, model, provable, tactic)
      // backup proof to prevent data loss
      backupProof(model, provable, tactic)
      new ProofVerificationResponse(proofId, provable, tree.tacticString) :: Nil
    }
  }
}

class IsLicenseAcceptedRequest(db : DBAbstraction) extends Request with ReadRequest {
  def resultingResponses(): List[Response] = {
    new BooleanResponse(
      db.getConfiguration("license").config.contains("accepted") &&
      db.getConfiguration("license").config("accepted") == "true"
    ) :: Nil
  }
}

class AcceptLicenseRequest(db : DBAbstraction) extends Request with WriteRequest {
  def resultingResponses(): List[Response] = {
    val newConfiguration = new ConfigurationPOJO("license", Map("accepted" -> "true"))
    db.updateConfiguration(newConfiguration)
    new BooleanResponse(true) :: Nil
  }
}

class RunScalaFileRequest(db: DBAbstraction, proofId: String, proof: File) extends LocalhostOnlyRequest with WriteRequest {
  override def resultingResponses(): List[Response] = ???
}

/////
// Requests for shutting down KeYmaera if KeYmaera is hosted locally.
/////

class IsLocalInstanceRequest() extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = new BooleanResponse(!HyDRAServerConfig.isHosted) :: Nil
}

class ExtractDatabaseRequest() extends LocalhostOnlyRequest with RegisteredOnlyRequest {
  override def resultingResponses(): List[Response] = {
    if (HyDRAServerConfig.isHosted) new ErrorResponse("Cannot extract the database on a hosted instance of KeYmaera X") :: Nil
    else {
      val productionDatabase = edu.cmu.cs.ls.keymaerax.hydra.SQLite.ProdDB
      productionDatabase.syncDatabase()

      val today = Calendar.getInstance().getTime
      val fmt = new SimpleDateFormat("MDY")

      val extractionPath = System.getProperty("user.home") + File.separator + s"extracted_${fmt.format(today)}.sqlite"
      val dbPath = productionDatabase.dblocation

      val src = new File(dbPath)
      val dest = new File(extractionPath)
      new FileOutputStream(dest).getChannel.transferFrom(
        new FileInputStream(src).getChannel, 0, Long.MaxValue)


      //@todo Maybe instead do this in the production database and then have a catch all that undoes it.
      //That way we don't have to sync twice. Actually, I'm also not sure if this sync is necessary or not...
      val extractedDatabase = new SQLiteDB(extractionPath)
      extractedDatabase.updateConfiguration(new ConfigurationPOJO("extractedflag", Map("extracted" -> "true")))
      extractedDatabase.syncDatabase()

      new ExtractDatabaseResponse(extractionPath) :: Nil
    }
  }
}

class ShutdownReqeuest() extends LocalhostOnlyRequest with RegisteredOnlyRequest {
  override def resultingResponses() : List[Response] = {
    new Thread() {
      override def run(): Unit = {
        try {
          //Tell all scheduled tactics to stop.
          //@todo figure out which of these are actually necessary.
          System.out.flush()
          System.err.flush()
          ToolProvider.shutdown()
          System.out.flush()
          System.err.flush()
          HyDRAServerConfig.system.terminate()
          System.out.flush()
          System.err.flush()
          this.synchronized {
            this.wait(4000)
          }
          System.out.flush()
          System.err.flush()
          System.exit(0) //should've already stopped the application by now.
        }
        catch {
          case _ : Exception => System.exit(-1)
        }

      }
    }.start()

    new BooleanResponse(true) :: Nil
  }
}

class ExtractTacticRequest(db: DBAbstraction, userName: String, proofIdStr: String) extends UserProofRequest(db, userName, proofIdStr) with WriteRequest {
  override def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofIdStr)
    val tactic = tree.tacticString
    // remember tactic string
    val newInfo = ProofPOJO(tree.info.proofId, tree.info.modelId, tree.info.name, tree.info.description,
      tree.info.date, tree.info.stepCount, tree.info.closed, tree.info.provableId, tree.info.temporary,
      Some(tactic))
    db.updateProofInfo(newInfo)
    GetTacticResponse(DbProofTree(db, proofIdStr).tacticString) :: Nil
  }
}

class GetTacticRequest(db: DBAbstraction, userName: String, proofIdStr: String) extends UserProofRequest(db, userName, proofIdStr) with ReadRequest {
  override def doResultingResponses(): List[Response] = {
    val proofInfo = db.getProofInfo(proofIdStr)
    GetTacticResponse(proofInfo.tactic.getOrElse(BellePrettyPrinter(Idioms.nil))) :: Nil
  }
}

class TacticDiffRequest(db: DBAbstraction, oldTactic: String, newTactic: String) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = {
    val oldT = BelleParser(oldTactic)
    try {
      val newT = BelleParser(newTactic)
      val diff = TacticDiff.diff(oldT, newT)
      new TacticDiffResponse(diff) :: Nil
    } catch {
      case e: ParseException => new ParseErrorResponse(e.msg, e.expect, e.found, e.getDetails, e.loc, e) :: Nil
    }
  }
}

class ExtractLemmaRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    tree.load()
    val model = db.getModel(tree.info.modelId.get)
    val tactic = tree.tacticString
    val provable = tree.root.provable
    val evidence = Lemma.requiredEvidence(provable, ToolEvidence(List(
      "tool" -> "KeYmaera X",
      "model" -> model.keyFile,
      "tactic" -> tactic
    )) :: Nil)
    new ExtractProblemSolutionResponse(new Lemma(provable, evidence, Some(tree.info.name)).toString) :: Nil
  }
}

object ArchiveEntryPrinter {
  def archiveEntry(modelInfo: ModelPOJO, tactics:List[(String, String)], withComments: Boolean): String = {
    KeYmaeraXArchiveParser(modelInfo.keyFile) match {
      case (entry@KeYmaeraXArchiveParser.ParsedArchiveEntry(name, _, _, _, _, _, _, _, _)) :: Nil if name == "<undefined>" =>
        new KeYmaeraXArchivePrinter(withComments)(replaceInfo(entry, modelInfo.name, tactics))
      case (entry@KeYmaeraXArchiveParser.ParsedArchiveEntry(name, _, _, _, _, _, _, _, _)) :: Nil if name != "<undefined>" =>
        new KeYmaeraXArchivePrinter(withComments)(replaceInfo(entry, entry.name, tactics))
    }
  }

  private def replaceInfo(entry: ParsedArchiveEntry, entryName: String, tactics: List[(String, String)]): ParsedArchiveEntry = {
    entry.copy(name = entryName, tactics = tactics.map(e => (e._1, e._2, TactixLibrary.skip)))
  }
}

class ExtractProblemSolutionRequest(db: DBAbstraction, userId: String, proofId: String) extends UserProofRequest(db, userId, proofId) with ReadRequest {
  override protected def doResultingResponses(): List[Response] = {
    val tree = DbProofTree(db, proofId)
    val proofName = tree.info.name
    val tactic = tree.tacticString
    val model = db.getModel(tree.info.modelId.get)

    def getLemmas(model: ModelPOJO, tactic: String): List[(String, (Option[ModelPOJO], Option[ProofPOJO]))] = {
      val lemmaFinder = """useLemma\(\"([^\"]*)\"""".r
      val lemmaNames = lemmaFinder.findAllMatchIn(tactic).map(m => m.group(1))
      val lemmaModels = lemmaNames.map(n => n -> db.getModelList(userId).find(n == _.name))
      val lemmas = lemmaModels.map(m => m._1 -> (m._2 match {
        case Some(mp) => db.getProofsForModel(mp.modelId).filter(_.closed) match {
          case Nil => m._2 -> None
          case proofs => m._2 -> Some(proofs.maxBy(_.date))
        }
        case _ => m._2 -> None
      })).toList
      val parentLemmas: List[(String, (Option[ModelPOJO], Option[ProofPOJO]))] = lemmas.flatMap({ case (_, (mp, pp)) => (mp, pp) match {
        case (Some(m), Some(p)) => p.tactic match {
          case Some(t) => getLemmas(m, t)
          case _ => Nil
        }
        case _ => Nil
      }})

      parentLemmas ++ lemmas
    }

    val lemmas = getLemmas(model, tactic)
    val printedLemmas = lemmas.map({
      case (_, (Some(modelPOJO), proofPOJO)) =>
        ArchiveEntryPrinter.archiveEntry(modelPOJO,
          proofPOJO match {
            case Some(p) => (p.name -> p.tactic.getOrElse("/* todo */ nil")) :: Nil
            case None => ("Todo" -> "/* todo */ nil") :: Nil
          },
          withComments = true)
      case (lemmaName, (None, _)) => s"""Lemma "$lemmaName" /* todo */ End."""
    })
    val modelContent = ArchiveEntryPrinter.archiveEntry(model, (proofName, tactic) :: Nil, withComments = true)
    val archiveContent = printedLemmas.mkString("\n\n") ++ modelContent
    new ExtractProblemSolutionResponse(archiveContent) :: Nil
  }
}

class ExtractModelSolutionsRequest(db: DBAbstraction, userId: String, modelIds: List[Int],
                                   withProofs: Boolean, exportEmptyProof: Boolean) extends UserRequest(userId) with ReadRequest {
  override def resultingResponses(): List[Response] = {
    def modelProofs(modelId: Int): List[(String, String)] = {
      if (withProofs) db.getProofsForModel(modelId).map(p =>
        p.name -> DbProofTree(db, p.proofId.toString).tacticString)
      else Nil
    }
    val models = modelIds.map(mid => db.getModel(mid) -> modelProofs(mid)).filter(exportEmptyProof || _._2.nonEmpty)
    val archiveContent = models.map({case (model, proofs) => ArchiveEntryPrinter.archiveEntry(model, proofs, withComments=true)}).mkString("\n\n")
    new ExtractProblemSolutionResponse(archiveContent + "\n") :: Nil
  }
}

class MockRequest(resourceName: String) extends Request {
  override def resultingResponses(): List[Response] = new MockResponse(resourceName) :: Nil
}

//region Proof validation requests

/** Global server state for proof validation requests.
  * For now, scheduling immediately dispatches a new thread where the validation occurs. In the future, we may want
  * to rate-limit validation requests. The easiest way to do that is to create a thread pool with a max size. */
object ProofValidationRunner extends Logging {
  private val results : mutable.Map[String, (Formula, BelleExpr, Option[Boolean])] = mutable.Map()

  case class ValidationRequestDNE(taskId: String) extends Exception(s"The requested taskId $taskId does not exist.")

  /** Returns Option[Proved] which is None iff the task is still running, and True if formula didn't prove. */
  def status(taskId: String) : Option[Boolean] = results.get(taskId) match {
    case Some((_, _, proved)) => proved
    case None => throw ValidationRequestDNE(taskId)
  }

  /** Schedules a proof validation request and returns the UUID. */
  def scheduleValidationRequest(db : DBAbstraction, model : Formula, proof : BelleExpr) : String = {
    val taskId = java.util.UUID.randomUUID().toString
    results update (taskId, (model, proof, None))

    new Thread(new Runnable() {
      override def run(): Unit = {
        logger.trace(s"Received request to validate $taskId. Running in separate thread.")
        val provable = ElidingProvable( Provable.startProof(model) )

        try {
          BelleInterpreter(proof, BelleProvable(provable)) match {
            case BelleProvable(p, _) if p.isProved => results update (taskId, (model, proof, Some(true )))
            case _                                 => results update (taskId, (model, proof, Some(false)))
          }
        } catch {
          //Catch everything and indicate a failed proof attempt.
          case e : Throwable => results update (taskId, (model, proof, Some(false)))
        }

        logger.trace(s"Done executing validation check for $taskId")
      }
    }).start()

    taskId
  }
}

/** Returns a UUID whose status can be queried at a later time ({complete: true/false[, proves: true/false]}.
  * @see CheckValidationRequest - calling this with the returned UUID should give the status of proof checking. */
class ValidateProofRequest(db : DBAbstraction, model: Formula, proof: BelleExpr) extends Request with ReadRequest {
  override def resultingResponses() : List[Response] =
    //Spawn an async validation request and return the reesulting UUID.
    new ValidateProofResponse(ProofValidationRunner.scheduleValidationRequest(db, model, proof), None) :: Nil
}

/** An idempotent request for the status of a validation request; i.e., validation requests aren't removed until the server is resst. */
class CheckValidationRequest(db: DBAbstraction, taskId: String) extends Request with ReadRequest {
  override def resultingResponses(): List[Response] = try {
    new ValidateProofResponse(taskId, ProofValidationRunner.status(taskId)) :: Nil
  } catch {
    case e : ProofValidationRunner.ValidationRequestDNE => new ErrorResponse(e.getMessage, e) :: Nil
  }
}

//endregion

object RequestHelper {

  def printDomain(d: Sort): String = d match {
    case Real => "R"
    case Bool => "B"
    case Unit => ""
    case Tuple(l, r) => printDomain(l) + "," + printDomain(r)
  }

  def augmentDeclarations(content: String, parsedContent: Formula): String =
    if (content.contains("Problem")) content //@note determine by mandatory "Problem" block of KeYmaeraXArchiveParser
    else {
      val symbols = StaticSemantics.symbols(parsedContent)
      val fnDecls = symbols.filter(_.isInstanceOf[Function]).map(_.asInstanceOf[Function]).map(fn =>
        if (fn.sort == Real) s"R ${fn.asString}(${printDomain(fn.domain)})."
        else if (fn.sort == Bool) s"B ${fn.asString}(${printDomain(fn.domain)})."
        else ???
      ).mkString("\n  ")
      val varDecls = symbols.filter(_.isInstanceOf[BaseVariable]).map(v => s"R ${v.prettyString}.").mkString("\n  ")
      s"""Functions.
         |  $fnDecls
         |End.
         |ProgramVariables.
         |  $varDecls
         |End.
         |Problem.
         |  $content
         |End.""".stripMargin
    }

  def jsonDisplayInfoComponents(di: ProvableInfo): JsValue = {
    val keyPos = AxIndex.axiomIndex(di)._1

    //@todo need more verbose axiom info
    ProvableInfo.locate(di.canonicalName) match {
      case Some(i) =>
        val (cond, op, key, keyPosString, conclusion, conclusionPos) = i.provable.conclusion.succ.head match {
          case Imply(c, eq@Equiv(l, r)) if keyPos == PosInExpr(1::0::Nil) => (Some(c), OpSpec.op(eq).opcode, l, "1.0", r, "1.1")
          case Imply(c, eq@Equiv(l, r)) if keyPos == PosInExpr(1::1::Nil) => (Some(c), OpSpec.op(eq).opcode, r, "1.1", l, "1.0")
          case bcf: BinaryCompositeFormula if keyPos == PosInExpr(0::Nil) => (None, OpSpec.op(bcf).opcode, bcf.left, "0", bcf.right, "1")
          case bcf: BinaryCompositeFormula if keyPos == PosInExpr(1::Nil) => (None, OpSpec.op(bcf).opcode, bcf.right, "1", bcf.left, "0")
          case f => (None, OpSpec.op(Equiv(f, True)).opcode, f, "0", True, "1")
        }
        JsObject(
          "cond" -> (if (cond.isDefined) JsString(UIKeYmaeraXAxiomPrettyPrinter.pp(cond.get)) else JsNull),
          "op" -> (if (op.nonEmpty) JsString(UIKeYmaeraXAxiomPrettyPrinter.pp.htmlEncode(op)) else JsNull),
          "key" -> JsString(UIKeYmaeraXAxiomPrettyPrinter.pp(key)),
          "keyPos" -> JsString(keyPosString),
          "conclusion" -> JsString(UIKeYmaeraXAxiomPrettyPrinter.pp(conclusion)),
          "conclusionPos" -> JsString(conclusionPos)
        )
      case None => JsNull
    }
  }

  /* String representation of the actual step (if tacticId refers to stepAt, otherwise tacticId).
     For display purposes only. */
  def getSpecificName(tacticId: String, sequent:Sequent, l1: Option[PositionLocator], l2: Option[PositionLocator],
                      what: DerivationInfo => String): String =  getSpecificName(tacticId, sequent, l1, l2) match {
    case Left(s) => s
    case Right(t) => what(t)
  }

  /* Try to figure out the most intuitive inference rule to display for this tactic. If the user asks us "StepAt" then
   * we should use the StepAt logic to figure out which rule is actually being applied. Otherwise just ask TacticInfo */
  def getSpecificName(tacticId: String, sequent:Sequent, l1: Option[PositionLocator], l2: Option[PositionLocator]): Either[String,DerivationInfo] = {
    val pos = l1 match {case Some(Fixed(p, _, _)) => Some(p) case _ => None}
    val pos2 = l2 match {case Some(Fixed(p, _, _)) => Some(p) case _ => None}
    tacticId.toLowerCase match {
      case ("step" | "stepat") if pos.isDefined && pos2.isEmpty =>
        sequent.sub(pos.get) match {
          case Some(fml: Formula) =>
            UIIndex.theStepAt(fml, pos) match {
              case Some(step) => Right(DerivationInfo(step))
              case None => Left(tacticId)
            }
          case _ => Right(DerivationInfo.ofCodeName(tacticId))
        }
      case ("step" | "stepat") if pos.isDefined && pos2.isDefined =>
        sequent.sub(pos.get) match {
          case Some(fml: Formula) =>
            UIIndex.theStepAt(pos.get, pos2.get, sequent) match {
              case Some(step) => Right(DerivationInfo(step))
              case None => Left(tacticId)
            }
        }
      case _ => Right(DerivationInfo.ofCodeName(tacticId))
    }
  }

  /** A listener that stores proof steps in the database `db` for proof `proofId`. */
  def listenerFactory(db: DBAbstraction)(proofId: Int)(tacticName: String, parentInTrace: Int, branch: Int): Seq[IOListener] = {
    DBTools.listener(db, (tn: String) => {
      val codeName = tn.split("\\(").head
      Try(RequestHelper.getSpecificName(codeName, null, None, None, _ => tacticName)).getOrElse(tn)
    })(proofId)(tacticName, parentInTrace, branch)
  }

  /** Updates the definitions in `proofSession` to include the unexpanded symbols of the open goals in `node`. */
  def updateProofSessionDefinitions(proofSession: ProofSession, node: ProofTreeNode): ProofSession = {
    val signatures = node.children.flatMap(_.localProvable.subgoals.flatMap(StaticSemantics.signature)).toSet
    val undefined = signatures.filter(s => !proofSession.defs.asNamedSymbols.contains(s))
    val newDefs: Map[KeYmaeraXArchiveParser.Name, KeYmaeraXArchiveParser.Signature] = undefined.map({
      case Function(name, index, domain, sort, _) => (name, index) -> (Some(domain), sort, None, None, UnknownLocation)
      case ProgramConst(name, _) => (name, None) -> (None, Trafo, None, None, UnknownLocation)
      case u => (u.name, u.index) -> (None, u.sort, None, None, UnknownLocation) // should not happen
    }).toMap
    proofSession.copy(defs = proofSession.defs.copy(proofSession.defs.decls ++ newDefs))
  }

}
