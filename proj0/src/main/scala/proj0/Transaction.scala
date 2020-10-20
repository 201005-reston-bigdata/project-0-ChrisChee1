package proj0

import java.util.Date

import org.bson.types.ObjectId
import sun.java2d.pipe.SpanShapeRenderer.Simple

case class Transaction(_id: ObjectId, sendAccount: Int, receiveAccount: Int, amount: Float, date: Date) {

  override def toString(): String =
  {
    f"Account ${sendAccount} sent $$${amount}%.2f to Account ${receiveAccount} on ${date}\n"
  }
}

object Transaction {

  def apply(sendAccount: Int, receiveAccount: Int, amount: Float, date: Date): Transaction =
    Transaction(new ObjectId(), sendAccount, receiveAccount, amount, date)
}