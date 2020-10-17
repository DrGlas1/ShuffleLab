package cards

object TestCompare {
    val deck = Deck.full()
    deck.shuffle()
    val player1 = Hand.from1(deck)
    val player2 = Hand.from2(deck)

    def main(args: Array[String]): Unit = {
        println(player1)
        println(player2)
        println(player1.isBetter(player1, player2))
        println(player1.highestCard(player1))
        println(player2.highestCard(player2))
    }
}