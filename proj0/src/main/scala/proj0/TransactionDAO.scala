package proj0

import org.mongodb.scala.{MongoClient, MongoCollection, Observable}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.Filters


import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

class TransactionDAO(mongoClient: MongoClient) {

  val codecRegistry = fromRegistries(fromProviders(classOf[Transaction]), MongoClient.DEFAULT_CODEC_REGISTRY)
  val db = mongoClient.getDatabase("bankdb").withCodecRegistry(codecRegistry)
  val collection : MongoCollection[Transaction] = db.getCollection("transactions")

  private def getResults[T](obs: Observable[T]): Seq[T] =
  {
    Await.result(obs.toFuture(), Duration(10, SECONDS))
  }

  def printResults[T](obs: Observable[T]): Unit =
  {
    getResults(obs).foreach(println(_))
  }

  def getTransactions(): Seq[Transaction] = getResults(collection.find())

  def findTransactions(accountNum: Int) =
  {
    getResults(collection.find(Filters.or(Filters.equal("sendAccount", accountNum),
                                          Filters.equal("receiveAccount", accountNum))))
  }

  def addTransaction(transaction: Transaction): Unit =
  {
    getResults(collection.insertOne(transaction))
  }

  def deleteAllTransactions(): Unit =
  {
    printResults(collection.deleteMany(Filters.exists("_id")))
  }

  def applyTransaction(transaction: Transaction, accountsDAO: AccountDAO): Boolean =
  {
    if (accountsDAO.findAccount(transaction.sendAccount).isEmpty)
    {
      println(s"Account ${transaction.sendAccount} does not exist. Transaction declined.")
      return false
    }

    if (accountsDAO.findAccount(transaction.receiveAccount).isEmpty)
    {
      println(s"Account ${transaction.receiveAccount} does not exist. Transaction declined.")
      return false
    }

    val sendAccount = accountsDAO.findAccount(transaction.sendAccount)(0)
    val receiveAccount = accountsDAO.findAccount(transaction.receiveAccount)(0)

    if (sendAccount.balance < transaction.amount)
    {
      println(s"\nAccount ${transaction.sendAccount} has insufficient balance.")
      println(f"Current balance: $$${accountsDAO.findAccount(transaction.sendAccount)(0).balance}")
      println(f"Amount transferring: $$${transaction.amount}")
      println("Transaction declined.\n")
      false
    }
    else
    {
      accountsDAO.changeBalance(sendAccount.accountNum, -transaction.amount)
      accountsDAO.changeBalance(receiveAccount.accountNum, transaction.amount)
      true
    }
  }
}
