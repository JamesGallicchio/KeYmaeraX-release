package edu.cmu.cs.ls.keymaerax.tacticsinterface

import edu.cmu.cs.ls.keymaerax.bellerophon._
import edu.cmu.cs.ls.keymaerax.core.Provable
import edu.cmu.cs.ls.keymaerax.hydra.{ProofPOJO, ExecutionStepPOJO, DBAbstraction, ExecutionStepStatus}
import edu.cmu.cs.ls.keymaerax.hydra.ExecutionStepStatus.ExecutionStepStatus

/**
  * Created by bbohrer on 11/20/15.
  */

class TraceRecordingListener(db: DBAbstraction,
                             proofId: Int,
                             executionId: Int,
                             initialSibling: Option[Int],
                             globalProvable:Provable,
                             alternativeOrder: Int, branch:Int,
                             recursive: Boolean,
                             ruleName: String) extends IOListener {
  class TraceNode (isFirstNode: Boolean){
    var id: Option[Int] = None
    var parent: TraceNode = null
    var sibling: Option[Int] = None
    var input: Provable = null
    var output: Provable = null
    var local: Provable = null
    var executable: BelleExpr = null
    var status: ExecutionStepStatus = null
    var reverseChildren: List[TraceNode] = Nil
    def children = reverseChildren.reverse
    /* This is generated by the DB, so it will not be present when we first create an object for the step. However,
       we need to set it once it has been generated so other steps can get the appropriate ID.
     */
    var stepId: Option[Int] = None
    val altOrder = if (isFirstNode) alternativeOrder else 0
    val branchLabel: String = null
    val branchOrder: Option[Int] = Some(branch)
    val userExe = isFirstNode

    var inputProvableId: Option[Int] = None
    var outputProvableId: Option[Int] = None
    var localProvableId: Option[Int] = None
    var executableId: Option[Int] = None

    def getInputProvableId:Int = {
      if (input != null && inputProvableId.isEmpty)
        inputProvableId = Some(db.serializeProvable(input))
      inputProvableId.get
    }

    def getOutputProvableId:Option[Int] = {
      if (output != null && outputProvableId.isEmpty)
        outputProvableId = Some(db.serializeProvable(output))
      outputProvableId
    }

    def getLocalProvableId:Option[Int] = {
      if (local != null && localProvableId.isEmpty)
        localProvableId = Some(db.serializeProvable(local))
      localProvableId
    }

    def getExecutableId:Int = {
      if (executable != null && executableId.isEmpty)
        executableId = Some(db.addBelleExpr(executable))
      executableId.get
    }

    def asPOJO: ExecutionStepPOJO = {
      val parentStep = if (parent == null) None else parent.stepId
      new ExecutionStepPOJO (stepId, executionId, sibling, parentStep, branchOrder,
        Option(branchLabel), alternativeOrder,status, getExecutableId, getInputProvableId, getOutputProvableId,
        getLocalProvableId, userExe, ruleName)
    }
  }

  var youngestSibling: Option[Int] = initialSibling
  var node: TraceNode = null
  var isDead: Boolean = false
  var nodesWritten: List[TraceNode] = Nil

  /* Debug info: Records how deep inside the tree of begin-end pairs we are */
  var depth: Int = 0
  def begin(v: BelleValue, expr: BelleExpr): Unit = {
    synchronized {
      depth = depth + 1
      if(isDead) return
      val parent = node
      node = new TraceNode(isFirstNode = parent == null)
      node.parent = parent
      node.sibling = youngestSibling
      node.executable = expr
      node.status = ExecutionStepStatus.Running

      if (parent != null) {
        parent.status = ExecutionStepStatus.DependsOnChildren
        parent.reverseChildren = node :: parent.reverseChildren
        if (recursive) {
          db.updateExecutionStatus(parent.stepId.get, parent.status)
        }
      } else {
        // Only reconstruct provables for the top-level because the meaning of "branch" can change inside a tactic
        node.input = v match {
          case BelleProvable(p, _) => globalProvable(p, branch)
        }
      }
      if (parent == null || recursive) {
        node.stepId = Some(db.addExecutionStep(node.asPOJO))
        nodesWritten = node :: nodesWritten
      }
    }
  }

  def end(v: BelleValue, expr: BelleExpr, result: Either[BelleValue, BelleError]): Unit = {
    synchronized {
      depth = depth - 1
      if(isDead) return
      val current = node
      node = node.parent
      youngestSibling = current.id
      current.status =
        result match {
          case Left(_) => ExecutionStepStatus.Finished
          case Right(_) => ExecutionStepStatus.Error
        }
      if (node != null && !recursive) return
      db.updateExecutionStatus(current.stepId.get, current.status)
      if (node == null) {
        result match {
          // Only reconstruct provables for the top-level because the meaning of "branch" can change inside a tactic
          case Left(BelleProvable(p, _)) =>
            current.output = globalProvable(p, branch)
            current.local = p
          case _ =>
        }
        if (current.output != null) {
          db.updateResultProvables(current.stepId.get, current.getOutputProvableId, current.getLocalProvableId)
          if (current.output.isProved) {
            val p = db.getProofInfo(proofId)
            val provedProof = new ProofPOJO(p.proofId, p.modelId, p.name, p.description, p.date, p.stepCount, closed = true)
            db.updateProofInfo(provedProof)
          }
        }
      }
    }
  }

  /** Called by HyDRA before killing the interpreter's thread. Updates the database to reflect that the computation
    * was interrupted. There are two race conditions to worry about here:
    * (1) kill() can race with a call to begin/end that was in progress when kill() started. This is resolved with
    * a mutex (synchronized{} blocks)
    * (2) An in-progress computation can race with a kill signal (sent externally after kill() is called). This is
    * resolved by setting a flag during kill() which turns future operations into a no-op. */
  def kill(): Unit = {
    synchronized {
      isDead = true
      nodesWritten.foreach(_.stepId.foreach{case id => db.updateExecutionStatus(id, ExecutionStepStatus.Aborted)})
    }
  }
}