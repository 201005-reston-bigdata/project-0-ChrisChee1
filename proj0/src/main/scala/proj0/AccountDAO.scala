package proj0

import org.mongodb.scala.{FindObservable, MongoClient, MongoCollection, Observable}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Updates._

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

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

  def getAccounts(): Seq[Account] = getResults(collection.find())

  def getAccountsByBal() =
  {
    getResults(collection.find().sort(Sorts.descending("balance")))
  }

  def getAccount(accountNum: Int) =
  {
      if (findAccount(accountNum).isEmpty)
        println(s"Account ${accountNum} does not exist")
      else
        println(findAccount(accountNum)(0).toString())
  }

  def getChecking() =
  {
    getResults(collection.find(Filters.equal("accountType", "Checking")))
  }

  def getSavings() =
  {
    getResults(collection.find(Filters.equal("accountType", "Savings")))
  }

  def addAccount(account: Account): Unit =
  {
    if (findAccount(account.accountNum).nonEmpty)
      println(s"Account ${account.accountNum} already exists. Account was not created.")
    else {
      getResults(collection.insertOne(account))
      println(s"Account ${account.accountNum} successfully added.")
    }
  }

  def deleteAccount(accountNum: Int) =
  {
    if (findAccount(accountNum).isEmpty)
      println(s"Account ${accountNum} does not exist.")
    else {
      getResults(collection.deleteOne(Filters.equal("accountNum", accountNum)))
      println(s"Account ${accountNum} has been deleted.")
    }
  }

  def deleteAllAccounts(): Unit =
  {
    printResults(collection.deleteMany(Filters.exists("_id")))
  }

  def findAccount(accountNum: Int): Seq[Account] =
  {
    getResults(collection.find(Filters.equal("accountNum", accountNum)))
  }

  def changeBalance(accountNum: Int, amount: Float) =
  {
    getResults(collection.updateOne(equal("accountNum", accountNum),
      set("balance", findAccount(accountNum)(0).balance + amount)))
  }

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
