package cards


object PlayPoker {
    val deck = Deck.full()
    deck.shuffle()
    val player1 = Hand.from1(deck)
    val player2 = Hand.from2(deck)
    val result = player1.isBetter(player1, player2)

    def thinking: Unit = {
        Thread.sleep(500); print(".")
        Thread.sleep(500); print(".")
        Thread.sleep(500); print(".")
        Thread.sleep(500); print(".")
        Thread.sleep(500); print(".")
    }

    def victoryMessage(result: Int, player1: Hand = player1, player2: Hand = player2): Unit = {
        if (result == 1) {
            println(s"""
            |Player 1 had the better hand!
            |They had ${player1.whatBullshitDidTheyGet(player1)}
            |
            |Player 2 had ${player1.whatBullshitDidTheyGet(player2)}""".stripMargin)
        }
        else if (result == -1) {
            println(s"""
            |Player 2 had the better hand!
            |They had ${player1.whatBullshitDidTheyGet(player2)}
            |
            |Player 1 had ${player1.whatBullshitDidTheyGet(player1)}""".stripMargin)
        }
        else {
            println(s"""
            |It was a tie!
            |Player 1 had ${player1.whatBullshitDidTheyGet(player1)}
            |
            |Player 2 had ${player1.whatBullshitDidTheyGet(player2)}""".stripMargin)
        }
    }

    def main(args: Array[String]): Unit = {
        println(s"${player1}")
    }
}