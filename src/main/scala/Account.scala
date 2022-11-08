import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    // for project task 1.2: implement functions
    // for project task 1.3: change return type and update function bodies
    // removes an amount of money from the account, thread safe
    // returns Either with Left if successful, Right if not, no exceptions
    def withdraw(amount: Double): Either[String, RuntimeException] = {
        balance synchronized {
            // fail if we withdraw a negative amount
            if (amount <= 0) {
                return Right(new IllegalAmountException("Amount must be greater than 0"))
            }
            // fail if we request a withdrawal that is larger than the available funds
            else if (amount > getBalanceAmount) {
                return Right(new NoSufficientFundsException("Insufficient funds"))
            } else {
                decreaseBalanceAmount(amount)
                return Left("Success")
            }
        }
    }

    // decrease balance amount
    def decreaseBalanceAmount(amount: Double): Unit = {
        balance synchronized {
            balance.amount -= amount
        }
    }

    // increase balance amount
    def increaseBalanceAmount(amount: Double): Unit = {
        balance synchronized {
            balance.amount += amount
        }
    }

    // inserts an amount of money to the account, tread safe
    // returns Either with Left if successful, Right if not, no exceptions
    def deposit(amount: Double): Either[String, RuntimeException] = {
        balance synchronized {
            // fail if we deposit a negative amount
            if (amount <= 0) {
                return Right(new IllegalAmountException("Amount must be greater than 0"))
            } else {
                increaseBalanceAmount(amount)
                return Left("Success")
            }
        }
    }

    // returns the amount of funds in the account
    def getBalanceAmount: Double = balance.amount

    def transferTo(account: Account, amount: Double): Unit = {
        bank.addTransactionToQueue(this, account, amount)
    }


}
