import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    // TODO
    // for project task 1.2: implement functions
    // for project task 1.3: change return type and update function bodies
    def withdraw(amount: Double): Either[String, RuntimeException] = {
        balance synchronized {
            if (amount <= 0) {
                return Right(new IllegalAmountException("Amount must be greater than 0"))
            } else if (amount > getBalanceAmount) {
                return Right(new NoSufficientFundsException("Insufficient funds"))
            } else {
                decreaseBalanceAmount(amount)
                return Left("Success")
            }
        }
    }

    def decreaseBalanceAmount(amount: Double): Unit = {
        balance synchronized {
            balance.amount -= amount
        }
    }

    def increaseBalanceAmount(amount: Double): Unit = {
        balance synchronized {
            balance.amount += amount
        }
    }

    def deposit(amount: Double): Either[String, RuntimeException] = {
        balance synchronized {
            if (amount <= 0) {
                return Right(new IllegalAmountException("Amount must be greater than 0"))
            } else {
                increaseBalanceAmount(amount)
                return Left("Success")
            }
        }
    }

    def getBalanceAmount: Double = balance.amount

    def transferTo(account: Account, amount: Double): Unit = {
        bank addTransactionToQueue(this, account, amount)
    }


}
