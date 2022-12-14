import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
    val SUCCESS, PENDING, FAILED = Value
}

// Datastructure to hold transactions.
// Functions of TransactionQueue are implemented in a thread-safe manner.
class TransactionQueue {

    // project task 1.1
    // Add datastructure to contain the transactions
    private val _transactions = mutable.Queue[Transaction]()

    // Remove and return the first element from the queue
    def pop: Transaction = {
        _transactions.synchronized({
            _transactions.dequeue()
        })
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = {
        _transactions.synchronized({
            _transactions.isEmpty
        })
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = {
        _transactions.synchronized({
            _transactions.enqueue(t)
        })
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = {
        _transactions.synchronized({
            _transactions.head
        })
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = {
        _transactions.synchronized({
            _transactions.iterator
        })
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

    var status: TransactionStatus.Value = TransactionStatus.PENDING
    var attempt = 0

    def incrementAttempt(): Unit = {
        attempt += 1
    }

    override def run(): Unit = {

        def doTransaction(): Unit = {
            // test if the transaction is valid
            if (from withdraw amount isLeft) {
                // if valid, execute the transaction
                if (to deposit amount isLeft) {
                    status = TransactionStatus.SUCCESS
                }
                else {
                    // if the deposit fails, roll back the withdrawal
                    from deposit amount
                    incrementAttempt()
                    if (attempt == allowedAttemps) {
                        status = TransactionStatus.FAILED
                    }
                }
            } else {
                // transaction failed. Increment attempt and check if it is allowed
                incrementAttempt()
                if (attempt == allowedAttemps) {
                    status = TransactionStatus.FAILED
                }
            }
        }

        // make the code below thread safe
        if (status == TransactionStatus.PENDING) {
            doTransaction()
            Thread.sleep(50)
            // you might want this to make more room for
            // new transactions to be added to the queue
        }


    }
}
