package cards

object TexasHoldem {

    var player1bet = 0
    var player2bet = 0

    var player1Pool = 10000
    var player2Pool = 10000

    val deck = Deck.full()
    deck.shuffle()
    val player1 = Hand.draw1(deck)
    val player2 = Hand.draw2(deck)
    val flopp = Hand.flopp(deck)
    val turn = Hand.turn(deck)
    val river = Hand.river(deck)

    def continuingBettingPhase: Unit = {     
        (player2bet > player1bet) match {
            case true => {
                player1bet += scala.io.StdIn.readLine("Player 1's bet: ").toInt
                println(player1bet)
            }
            case false => 
        }
        (player2bet < player1bet) match {
            case true => {
                player2bet += scala.io.StdIn.readLine("Player 2's bet: ").toInt
                println(player1bet)
                continuingBettingPhase
            }
            case false => 
        }
    }

    def bettingPhase: Unit = {
            player1bet += scala.io.StdIn.readLine("Player 1's bet: ").toInt
            println(player1bet)
            player2bet += scala.io.StdIn.readLine("Player 2's bet: ").toInt
            println(player2bet)

        (player2bet == player1bet) match {
            case true =>
            case false => continuingBettingPhase
        }
    }

    def main(args: Array[String]): Unit = {    
        println("""
        |
        |First Player 1's hand will be shown for 10s, 
        |then Player 2's hand will be shown for 10s.
        |
        |After that the betting phase will begin!""".stripMargin)

        Thread.sleep(10000)

        println("\nPLAYER 2 TURN AWAY NOW")

        Thread.sleep(3000)
        
        println(s"""
        |
        |
        |
        |Player 1's hand: 
        |${player1.cards(0)}, ${player1.cards(1)}
        |
        |
        |""".stripMargin)

        Thread.sleep(10000)

        println("TURN AWAY NOW")

        Thread.sleep(3000)

        println(s"""
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |Player 2's hand:
        |${player2.cards(0)}, ${player2.cards(1)}
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |""".stripMargin)

        Thread.sleep(10000)

        println("""
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |""".stripMargin)

        bettingPhase 

        Thread.sleep(5000)

        println("""
        |
        |
        |""".stripMargin)

        println(s"The flopp is : \n\n\n ${flopp.cards(0)}, ${flopp.cards(1)}, ${flopp.cards(2)}")

        Thread.sleep(10000)

        println("""
        |
        |
        |""".stripMargin)

        println(s"The current pot is ${player1bet + player2bet}\n")

        bettingPhase

        println(s"The turn is : \n\n\n${flopp.cards(0)}, ${flopp.cards(1)}, ${flopp.cards(2)}, ${turn.cards(0)}")

        Thread.sleep(2000)

        println("""
        |
        |
        |""".stripMargin)

        println(s"The current pot is ${player1bet + player2bet}\n")

        bettingPhase

        println("""
        |
        |
        |""".stripMargin)

        println(s"The river is : \n\n\n${flopp.cards(0)}, ${flopp.cards(1)}, ${flopp.cards(2)}, ${turn.cards(0)}, ${river.cards(0)}")
        
        Thread.sleep(2000)

        println("""
        |
        |
        |""".stripMargin)

        println(s"The current pot is ${player1bet + player2bet}\n")

        bettingPhase

        println("""
        |
        |
        |""".stripMargin)
    }

}