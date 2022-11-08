import scala.annotation.tailrec

class Bank(val allowedAttempts: Integer = 3) {

    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()

    // project task 2
    // create a new transaction object and put it in the queue
    // spawn a thread that calls processTransactions
    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        val transaction = new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
        transactionsQueue.push(transaction)
        // start processing transactions concurrently.
        val thread = new Thread {
            override def run(): Unit = processTransactions()
        }
        thread.start()
    }


    // project task 2
    // Function that pops a transaction from the queue
    // and spawns a thread to execute the transaction.
    // Finally do the appropriate thing, depending on whether
    // the transaction succeeded or not
    @tailrec
    private def processTransactions(): Unit = {
        val transaction = transactionsQueue.pop
        transaction.run()
        // If a transactions’ status is pending, push it back to the queue and recursively call processTransactions
        if (transaction synchronized {
            transaction.status == TransactionStatus.PENDING
        }) {
            transactionsQueue.push(transaction)
            processTransactions()
        } else {
            processedTransactions.push(transaction)
        }

    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
