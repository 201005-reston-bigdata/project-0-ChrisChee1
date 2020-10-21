package proj0

import org.mongodb.scala.{MongoClient, MongoCollection, Observable}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.{Filters, Sorts}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

/** Data Access Object for the Transaction class */
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

  /** Returns Sequence containing all Transactions */
  def getTransactions(): Seq[Transaction] = getResults(collection.find())

  /** Returns Sequence containing all Transactions in descending order of amount */
  def getTransactionsByAmt() =
  {
    getResults(collection.find().sort(Sorts.descending("amount")))
  }

  /** Returns Sequence containing all Transactions in descending order of date */
  def getTransactionsByDate() =
  {
    getResults(collection.find().sort(Sorts.descending("date")))
  }

  /** Returns Sequence of Transactions involving Account corresponding to account number passed as argument */
  def findTransactions(accountNum: Int) =
  {
    getResults(collection.find(Filters.or(Filters.equal("sendAccount", accountNum),
                                          Filters.equal("receiveAccount", accountNum))))
  }

  /** Inserts Transaction passed as argument into MongoDB database */
  def addTransaction(transaction: Transaction): Unit =
  {
    getResults(collection.insertOne(transaction))
  }

  /** Deletes all Transactions in MongoDB database */
  def deleteAllTransactions(): Unit =
  {
    printResults(collection.deleteMany(Filters.exists("_id")))
  }

  /** Applies balance changes in Transaction passed as argument to corresponding Accounts */
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
      println(f"Current balance: $$${accountsDAO.findAccount(transaction.sendAccount)(0).balance}%.2f")
      println(f"Amount transferring: $$${transaction.amount}%.2f")
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