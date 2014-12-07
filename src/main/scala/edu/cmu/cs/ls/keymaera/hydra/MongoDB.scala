package edu.cmu.cs.ls.keymaera.hydra

import com.mongodb.casbah.Imports._
import edu.cmu.cs.ls.keymaera.api.KeYmaeraInterface
import edu.cmu.cs.ls.keymaera.api.KeYmaeraInterface.PositionTacticAutomation
import org.bson.types.ObjectId

import scala.collection.mutable.ListBuffer

object MongoDB extends DBAbstraction {
  val mongoClient = MongoClient("localhost", 27017)

  //Collections.
  val users   = mongoClient("keymaera")("users")
  val models  = mongoClient("keymaera")("models")
  val proofs  = mongoClient("keymaera")("proofs")
  val logs    = mongoClient("keymaera")("logs")
  val tactics = mongoClient("keymaera")("tactics")

  def cleanup() = {
    users.drop()
    models.drop()
    proofs.drop()
    logs.drop()
    tactics.drop()
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Users
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override def getUsername(uid: String): String = uid

  override def userExists(username : String) = {
    val query = MongoDBObject("username" -> username)
    users.count(query) > 0
  }

  override def createUser(username: String, password: String): Unit = {
    val newUser = MongoDBObject("username" -> username, "password" -> password)
    users.insert(newUser)
  }

  override def checkPassword(username: String, password: String): Boolean = {
    val result = MongoDBObject("username" -> username, "password" -> password)
    users.count(result) > 0
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Models
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  override def createModel(userId: String, name: String, fileContents: String, currentDate : String): Boolean = {
    val query = MongoDBObject("userId" -> userId, "name" -> name, "fileContents" -> fileContents) //w/o date
    if (models.find(query).length == 0) {
      val obj = MongoDBObject("userId" -> userId, "name" -> name, "date" -> currentDate, "fileContents" -> fileContents)
      models.insert(obj)
      true // TODO should we allow models by the same user and with the same name?
    }
    else {
      false
    }
  }

  override def getModelList(userId : String) = {
    val query = MongoDBObject("userId" -> userId)
    val allModels = models.find(query)
    allModels.map(model =>
      if(!model.containsField("_id")) throw new IllegalStateException("Model without ID")
      else new ModelPOJO(
        model.getAs[ObjectId]("_id").orNull.toString,
        model.getAs[String]("userId").orNull.toString,
        model.getAs[String]("name").getOrElse("") ,
        model.getAs[String]("date").getOrElse(""),
        model.getAs[String]("fileContents").getOrElse("")
      )
    ).toList
  }

  override def getModel(modelId: String): ModelPOJO = {
    val query = MongoDBObject("_id" -> new ObjectId(modelId))
    val results = models.find(query)
    if(results.length > 1) throw new IllegalStateException("Duplicate model ID: " + modelId)
    if(results.length < 1) throw new Exception(modelId + " is a bad modelId!")
    results.map(result => new ModelPOJO(
      result.getAs[ObjectId]("_id").orNull.toString,
      result.getAs[String]("userId").orNull.toString,
      result.getAs[String]("name").getOrElse("") ,
      result.getAs[String]("date").getOrElse(""),
      result.getAs[String]("fileContents").getOrElse("")
    )).toList.last
  }

  override def createProofForModel(modelId : String, name : String, description : String, date:String) : String = {
    val builder = MongoDBObject.newBuilder
    builder += "modelId" -> new ObjectId(modelId)
    builder += "name" -> name
    builder += "description" -> description
    builder += "date" -> date
    builder += "dispatchedTactics" -> List()
    builder += "completedSteps" -> List()
    val query = builder.result()

    proofs.insert(query)
    query.get("_id").toString
  }

  override def getProofsForModel(modelId : String) : List[ProofPOJO] = {
    val query = MongoDBObject("modelId" -> new ObjectId(modelId))
    val results = proofs.find(query)
    if(results.length < 1) Nil
    else {
      results.map(result => {
        new ProofPOJO(
          result.getAs[ObjectId]("_id").orNull.toString,
          result.getAs[ObjectId]("modelId").orNull.toString,
          result.getAs[String]("name").orNull.toString,
          result.getAs[String]("description").getOrElse("no description"),
          result.getAs[String]("date").orNull.toString,
          result.getAs[Integer]("stepCount").getOrElse(0),
          result.getAs[Boolean]("closed").getOrElse(false)
        )
      }).toList
    }
  }

  override def openProofs(userId : String) : List[ProofPOJO] = {
    //return all the non-closed proofs.
    getProofsForUser(userId).map(_._1).filter(!_.closed)
  }

  override def getProofsForUser(userId : String) : List[(ProofPOJO, String)] = {
    val models = getModelList(userId)

    models.map(model => {
      val modelName = model.name
      val proofs = getProofsForModel(model.modelId)
      proofs.map((_, modelName))
    }).flatten
  }

  override def getProofInfo(proofId: String): ProofPOJO = {
    var query = MongoDBObject("_id" -> new ObjectId(proofId))
    val results = proofs.find(query)
    if(results.length > 1) throw new IllegalStateException("Proof ID " + proofId + " is not unique") //There should only be one response b/c _id is a pk.
    if(results.length < 1) throw new IllegalArgumentException(proofId + " is a bad proofId!")
    results.map(result => new ProofPOJO(
      result.getAs[ObjectId]("_id").orNull.toString,
      result.getAs[ObjectId]("modelId").orNull.toString,
      result.getAs[String]("name").getOrElse(""),
      result.getAs[String]("description").getOrElse("") ,
      result.getAs[String]("date").getOrElse(""),
      result.getAs[Integer]("stepCount").getOrElse(0),
      result.getAs[Boolean]("closed").getOrElse(false)
    )).toList.last
  }

  override def updateProofInfo(proof: ProofPOJO) = {
    val update = $set(
      "modelId"     -> new ObjectId(proof.modelId),
      "name"        -> proof.name,
      "description" -> proof.description,
      "date"        -> proof.date,
      "stepCount"   -> proof.stepCount,
      "closed"      -> proof.closed
    )
    proofs.update(MongoDBObject("_id" -> new ObjectId(proof.proofId)), update)
  }

  override def getProofSteps(proofId: String) : List[String] = {
    val query = MongoDBObject("_id" -> new ObjectId(proofId))
    val results = proofs.find(query)
    if(results.length > 1) throw new IllegalStateException("Proof ID " + proofId + " is not unique")
    if(results.length < 1) throw new IllegalArgumentException(proofId + " is a bad proofId!")
    results.one().getAs[List[ObjectId]]("completedSteps").getOrElse(List[ObjectId]()).map(_.toString)
  }

  override def addFinishedTactic(proofId: String, tacticInstId: String) = {
    val query = MongoDBObject("_id" -> new ObjectId(proofId))
    val results = proofs.find(query)
    if(results.length > 1) throw new IllegalStateException("Proof ID " + proofId + " is not unique")
    if(results.length < 1) throw new IllegalArgumentException(proofId + " is a bad proofId!")
    val update = $push("completedSteps" -> new ObjectId(tacticInstId))
    proofs.update(query, update)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tactics
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def createTactic(name : String, clazz : String, kind : TacticKind.Value) : String = {
    val query = MongoDBObject(
      "name"    -> name,
      "clazz"   -> clazz,
      "kind"    -> kind.toString
    )
    tactics.insert(query)
    query.get("_id").toString
  }
  def tacticExists(id : String) : Boolean = {
    if (ObjectId.isValid(id)) {
      val query = MongoDBObject("_id" -> new ObjectId(id))
      tactics.count(query) > 0
    } else {
      false
    }
  }
  def getTactic(id: String) : Option[TacticPOJO] = {
    val query = MongoDBObject("_id" -> new ObjectId(id))
    val results = tactics.find(query)
    if(results.length > 1) throw new IllegalStateException("Tactic ID " + id + " is not unique")
    if(results.length < 1) None
    Some(results.map(result => new TacticPOJO(
      result.getAs[ObjectId]("_id").orNull.toString,
      result.getAs[String]("name").getOrElse(""),
      result.getAs[String]("clazz").getOrElse(""),
      TacticKind.withName(result.getAs[String]("kind").getOrElse(""))
    )).toList.last)
  }
  def getTacticByName(name: String) : Option[TacticPOJO] = {
    val query = MongoDBObject("name" -> name)
    val results = tactics.find(query)
    if(results.length > 1) throw new IllegalStateException("Duplicate tactic name: " + name)
    if (results.length < 1) None
    else Some(results.map(result => new TacticPOJO(
      result.getAs[ObjectId]("_id").orNull.toString,
      result.getAs[String]("name").getOrElse(""),
      result.getAs[String]("clazz").getOrElse(""),
      TacticKind.withName(result.getAs[String]("kind").getOrElse(""))
    )).toList.last)
  }
  override def getTactics : List[TacticPOJO] = {
    val results = tactics.find()
    results.map(result => new TacticPOJO(
      result.getAs[ObjectId]("_id").orNull.toString,
      result.getAs[String]("name").getOrElse(""),
      result.getAs[String]("clazz").getOrElse(""),
      TacticKind.withName(result.getAs[String]("kind").getOrElse(""))
    )).toList
  }

  override def createDispatchedTactics(proofId: String, nodeId: Option[String], formulaId: Option[String],
                                       tacticsId: String, input: Map[Int,String],
                                       auto: Option[PositionTacticAutomation.Value],
                                       status: DispatchedTacticStatus.Value): String = {
    val builder = MongoDBObject.newBuilder
    builder += "_id"       -> new ObjectId()
    builder += "proofId"   -> new ObjectId(proofId)
    builder += "tacticsId" -> new ObjectId(tacticsId)
    builder += "status"    -> status.toString
    builder += "input"     -> input.map({ case (k,v) => (k.toString, v) })
    if (nodeId.isDefined) builder += "nodeId" -> nodeId.get
    if (formulaId.isDefined) builder += "formulaId" -> formulaId.get
    if (auto.isDefined) builder += "auto" -> auto.toString
    val newDispatchedTactic = builder.result()

    val query = $push("dispatchedTactics" -> newDispatchedTactic)
    proofs.update(MongoDBObject("_id" -> new ObjectId(proofId)), query)
    newDispatchedTactic.get("_id").toString
  }

  override def getDispatchedTactics(tId : String) : Option[DispatchedTacticPOJO] = {
    proofs.findOne(MongoDBObject("dispatchedTactics._id" -> new ObjectId(tId)),
                   MongoDBObject("dispatchedTactics.$" -> 1)) match {
      case Some(r) =>
        r.getAs[BasicDBList]("dispatchedTactics") match {
          case Some(dtl) =>
            Some(dtl.map(_.asInstanceOf[BasicDBObject]).map(dt =>
              new DispatchedTacticPOJO(
                dt.getAs[ObjectId]("_id").orNull.toString,
                dt.getAs[ObjectId]("proofId").orNull.toString,
                if (dt.containsField("nodeId")) Some(dt.getAs[String]("nodeId").get) else None,
                if (dt.containsField("formulaId")) Some(dt.getAs[String]("formulaId").get) else None,
                dt.getAs[ObjectId]("tacticsId").orNull.toString,
                dt.getAs[Map[String,String]]("input").get.map({ case (k,v) => (k.toInt, v) }),
                if (dt.containsField("auto")) Some(PositionTacticAutomation.withName(dt.getAs[String]("auto").get))
                else None,
                DispatchedTacticStatus.withName(dt.getAs[String]("status").getOrElse(""))
            )).toList.head)
          case None => None
        }
      case None => None
    }
  }

  override def updateDispatchedTactics(tactic : DispatchedTacticPOJO) = {
    val builder = MongoDBObject.newBuilder
    builder += "_id"          -> new ObjectId(tactic.id)
    builder += "proofId"      -> new ObjectId(tactic.proofId)
    builder += "tacticsId"    -> new ObjectId(tactic.tacticsId)
    builder += "status"       -> tactic.status.toString
    builder += "input"        -> tactic.input.map({ case (k,v) => (k.toString, v) })
    if (tactic.nodeId.isDefined) builder += "nodeId" -> tactic.nodeId.get
    if (tactic.formulaId.isDefined) builder += "formulaId" -> tactic.formulaId.get
    if (tactic.auto.isDefined) builder += "auto" -> tactic.auto.get.toString
    val update = $set("dispatchedTactics.$" -> builder.result())

    val query = MongoDBObject("dispatchedTactics._id" -> new ObjectId(tactic.id))
    proofs.update(query, update)
  }

  override def updateProofOnTacticCompletion(proofId: String, tId: String): Unit = {
    val openGoals = KeYmaeraInterface.getOpenGoalCount(proofId)

    val query = MongoDBObject("dispatchedTactics._id" -> new ObjectId(tId))

    val results = proofs.find(query)
    if(results.length > 1) throw new IllegalStateException("Proof ID " + proofId + " is not unique")
    if(results.length < 1) throw new IllegalArgumentException(proofId + " is a bad proofId!")

    val update = $push("completedSteps" -> new ObjectId(tId)) ++ $set(
      // TODO update step count
      if (openGoals == 0) "closed" -> true else "closed" -> false,
      "dispatchedTactics.$.status" -> DispatchedTacticStatus.Finished.toString
    )

    proofs.update(query, update)
  }
}
