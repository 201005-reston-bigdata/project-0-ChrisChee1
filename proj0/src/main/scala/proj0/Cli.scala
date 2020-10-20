package proj0

import java.text.SimpleDateFormat
import java.util.Date

import org.mongodb.scala.MongoClient
import sun.java2d.pipe.SpanShapeRenderer.Simple

import scala.io.StdIn
import scala.util.matching.Regex

class Cli {

  val commandArgPattern : Regex = "(\\w+)\\s*(.*)".r
  val commandArgPattern2 : Regex = "(\\w+)\\s*(\\w+)\\s*(.*)".r

  def printOptions(): Unit =
  {
    println("Please enter a command: ")
    println("read accounts [filename]")
    println("read transactions [filename]")
    println("list accounts")
    println("list account [account number]")
    println("list transactions")
    println("list transactions [account number]")
    println("pay interest")
    println("delete accounts")
    println("delete account [account number]")
    println("delete transactions")
    println("exit")
  }

  def menu(): Unit =
  {
    val client = MongoClient()
    val accountsDAO = new AccountDAO(client)
    val transactionsDAO = new TransactionDAO(client)
    var loop = true

    while (loop)
    {
      printOptions()
      StdIn.readLine() match
      {
        case commandArgPattern2(cmd, fileType, arg)
          if (cmd.equalsIgnoreCase("read")) =>
          {
            if (fileType.equalsIgnoreCase("accounts"))
            {
              if (arg.substring(arg.length - 4).equalsIgnoreCase(".csv"))
                parseCSVAccounts (arg, accountsDAO)
              else if (arg.substring(arg.length - 5).equalsIgnoreCase(".json"))
                parseJSONAccounts (arg, accountsDAO)
              else
                println ("File type not supported")
            }
            else if (fileType.equalsIgnoreCase("transactions"))
            {
              if (arg.substring(arg.length - 4).equalsIgnoreCase(".csv"))
                parseCSVTransactions (arg, transactionsDAO, accountsDAO)
              else if (arg.substring(arg.length - 5).equalsIgnoreCase(".json"))
                parseJSONTransactions (arg, transactionsDAO, accountsDAO)
              else
                println ("File type not supported")
            }
          }
        case commandArgPattern2(cmd, fileType, arg)
          if (cmd.equalsIgnoreCase ("list")) =>
          {
            if (fileType.equalsIgnoreCase("account"))
            {
              accountsDAO.getAccount(arg.toInt)
            }
            else if (fileType.equalsIgnoreCase("accounts"))
              accountsDAO.getAccounts().foreach(account => println(s"${account}\n"))
            else if (fileType.equalsIgnoreCase("transactions"))
            {
              if (arg.equals(""))
                transactionsDAO.getTransactions().foreach(transaction => println(s"${transaction}\n"))
              else
              {
                val transactions = transactionsDAO.findTransactions(arg.toInt)

                if (transactions.isEmpty)
                {
                  println(s"No transactions involving Account ${arg} were found.")
                }
                else
                {
                  for (transaction <- transactions)
                    println(transaction)
                }
              }
            }
          }
        case commandArgPattern2(cmd, fileType, arg)
          if cmd.equalsIgnoreCase("delete") =>
          {
            if (fileType.equalsIgnoreCase("account"))
            {
              accountsDAO.deleteAccount(arg.toInt)
            }
            else if (fileType.equalsIgnoreCase("accounts"))
            {
              accountsDAO.deleteAllAccounts()
            }
            else if (fileType.equalsIgnoreCase("transactions"))
            {
              transactionsDAO.deleteAllTransactions()
            }
          }
        case commandArgPattern(cmd, arg)
          if cmd.equalsIgnoreCase("delete") =>
          {
            if (arg.equalsIgnoreCase("accounts"))
              accountsDAO.deleteAllAccounts()
            else if (arg.equalsIgnoreCase("transactions"))
              transactionsDAO.deleteAllTransactions()
          }
        case commandArgPattern(cmd, arg)
          if cmd.equalsIgnoreCase("pay") =>
          {
            if (arg.equalsIgnoreCase("interest"))
              accountsDAO.addInterest()
          }
        case commandArgPattern(cmd, arg)
          if cmd.equalsIgnoreCase("exit") =>
            loop = false
        case invalid => println(s"${invalid} not a recognized command")
      }
      println()
    }
  }

  def parseCSVAccounts(fileName: String, dao: AccountDAO): Unit =
  {
    val accountArray = FileUtil.getCSVText(fileName).getOrElse("None").split("\n")

    for (account <- accountArray)
    {
      val fields = account.split(",")
      dao.addAccount(Account(fields(0).toInt, fields(1), fields(2), fields(3), fields(4).toFloat, fields(5).toFloat))
    }
  }

  def parseCSVTransactions(fileName: String, transactionDAO: TransactionDAO, accountsDAO: AccountDAO): Unit =
  {
    val dateFormat = "yyyy/MM/dd"
    val simpleDF: SimpleDateFormat = new SimpleDateFormat(dateFormat)
    val transactionArray = FileUtil.getCSVText(fileName).getOrElse("None").split("\n")

    for (transaction <- transactionArray)
    {
      val fields = transaction.split(",")
      val newTransaction = Transaction(fields(0).toInt, fields(1).toInt, fields(2).toFloat, simpleDF.parse(fields(3)))

      if (transactionDAO.applyTransaction(newTransaction, accountsDAO))
      {
        println("Transaction completed successfully")
        transactionDAO.addTransaction(newTransaction)
      }
    }
  }

  def parseJSONAccounts(fileName: String, dao: AccountDAO): Unit =
  {
    val accountArray = FileUtil.getJSONText(fileName).getOrElse("None").split("},")

    for (account <- accountArray)
    {
      val fields = account.replaceAll("]", "").replaceAll("}", "").replaceAll("\\s", "").split(",")

      val accountNum = fields(0).substring(fields(0).indexOf(":") + 1).toInt
      val lastName = fields(1).substring(fields(1).indexOf(":") + 2, fields(1).length - 1)
      val firstName = fields(2).substring(fields(2).indexOf(":") + 2, fields(2).length - 1)
      val accountType = fields(3).substring(fields(3).indexOf(":") + 2, fields(3).length - 1)
      val balance = fields(4).substring(fields(4).indexOf(":") + 1).toFloat
      val interest = fields(5).substring(fields(5).indexOf(":") + 1).toFloat

      dao.addAccount(Account(accountNum, lastName, firstName, accountType, balance, interest))
    }
  }

  def parseJSONTransactions(fileName: String, transactionDAO: TransactionDAO, accountsDAO: AccountDAO): Unit =
  {
    val dateFormat = "yyyy/MM/dd"
    val simpleDF: SimpleDateFormat = new SimpleDateFormat(dateFormat)
    val transactionArray = FileUtil.getJSONText(fileName).getOrElse("None").split("},")

    for (transaction <- transactionArray)
    {
      val fields = transaction.replaceAll("]", "").replaceAll("}", "").replaceAll("\\s", "").split(",")

      val sendAccount = fields(0).substring(fields(0).indexOf(":") + 1).toInt
      val receiveAccount = fields(1).substring(fields(1).indexOf(":") + 1).toInt
      val amount = fields(2).substring(fields(2).indexOf(":") + 1).toFloat
      val date = simpleDF.parse(fields(3).substring(fields(3).indexOf(":") + 2, fields(3).length - 1))

      val newTransaction = Transaction(sendAccount, receiveAccount, amount, date)

      if (transactionDAO.applyTransaction(newTransaction, accountsDAO))
      {
        println("Transaction completed successfully")
        transactionDAO.addTransaction(newTransaction)
      }
    }
  }
}