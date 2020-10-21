package proj0

import org.mongodb.scala.{FindObservable, MongoClient, MongoCollection, Observable}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Updates._

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

/** Data Access Object for the Account class */
class AccountDAO(mongoClient: MongoClient) {

  val codecRegistry = fromRegistries(fromProviders(classOf[Account]), MongoClient.DEFAULT_CODEC_REGISTRY)
  val db = mongoClient.getDatabase("bankdb").withCodecRegistry(codecRegistry)
  val collection : MongoCollection[Account] = db.getCollection("accounts")

  private def getResults[T](obs: Observable[T]): Seq[T] =
  {
    Await.result(obs.toFuture(), Duration(10, SECONDS))
  }

  def printResults[T](obs: Observable[T]): Unit =
  {
    getResults(obs).foreach(println(_))
  }

  /** Returns Sequence containing all Accounts */
  def getAccounts(): Seq[Account] = getResults(collection.find())

  /** Returns Sequence containing all Accounts in descending order of balance */
  def getAccountsByBal() =
  {
    getResults(collection.find().sort(Sorts.descending("balance")))
  }

  /** Prints Account information corresponding to account number passed as argument */
  def getAccount(accountNum: Int) =
  {
      if (findAccount(accountNum).isEmpty)
        println(s"Account ${accountNum} does not exist")
      else
        println(findAccount(accountNum)(0).toString())
  }

  /** Returns Sequence containing all Checking type Accounts */
  def getChecking() =
  {
    getResults(collection.find(Filters.equal("accountType", "Checking")))
  }

  /** Returns Sequence containing all Savings type Accounts */
  def getSavings() =
  {
    getResults(collection.find(Filters.equal("accountType", "Savings")))
  }

  /** Inserts Account passed as argument into MongoDB database */
  def addAccount(account: Account): Unit =
  {
    if (findAccount(account.accountNum).nonEmpty)
      println(s"Account ${account.accountNum} already exists. Account was not created.")
    else {
      getResults(collection.insertOne(account))
      println(s"Account ${account.accountNum} successfully added.")
    }
  }

  /** Deletes Account corresponding to account number passed as argument from MongoDB database */
  def deleteAccount(accountNum: Int) =
  {
    if (findAccount(accountNum).isEmpty)
      println(s"Account ${accountNum} does not exist.")
    else {
      getResults(collection.deleteOne(Filters.equal("accountNum", accountNum)))
      println(s"Account ${accountNum} has been deleted.")
    }
  }

  /** Deletes all Accounts in MongoDB database */
  def deleteAllAccounts(): Unit =
  {
    printResults(collection.deleteMany(Filters.exists("_id")))
  }

  /** Returns Sequence containing Account corresponding to account number passed as argument */
  def findAccount(accountNum: Int): Seq[Account] =
  {
    getResults(collection.find(Filters.equal("accountNum", accountNum)))
  }

  /** Adds amount to balance of Account corresponding to account number passed as argument */
  def changeBalance(accountNum: Int, amount: Float) =
  {
    getResults(collection.updateOne(equal("accountNum", accountNum),
      set("balance", findAccount(accountNum)(0).balance + amount)))
  }

  /** Increases balance of all Savings Accounts by their interest rate */
  def addInterest(): Unit =
  {
    for (account <- getAccounts())
    {
      if (account.interest > 0)
      {
        println(s"Account ${account.accountNum}")
        println(f"Current Balance: $$${account.balance}%.2f")
        println(f"Interest Rate: ${account.interest}%.2f%%")
        println(f"Interest Paid: $$${account.balance * account.interest / 100}%.2f")
        changeBalance(account.accountNum, account.balance * account.interest / 100)
        println(f"New Balance: $$${findAccount(account.accountNum)(0).balance}%.2f\n")
      }
    }
  }
}
