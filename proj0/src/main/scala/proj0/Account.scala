package proj0

import org.bson.types.ObjectId

/** Case class to represent an Account in the application */
case class Account(_id: ObjectId, accountNum: Int, lastName: String, firstName: String, accountType: String, balance: Float, interest: Float) {

  override def toString(): String =
  {
    f"Account Number: ${accountNum}\nName: ${firstName} ${lastName}\nAccount Type: ${accountType}\nBalance: $$${balance}%.2f\nInterest Rate: ${interest}%.2f%%"
  }
}

object Account {
  def apply(accountNum: Int, lastName: String, firstName: String, accountType: String, balance: Float, interest: Float): Account =
    Account(new ObjectId(), accountNum, lastName, firstName, accountType, balance, interest)
}
