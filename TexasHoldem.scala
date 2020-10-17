package cards

object TexasHoldem {

    var player1Bet = 0
    var player2Bet = 0
    var blindCounter = 1
    var smallBlind = 50 + 50 * (blindCounter / 5)
    var bigBlind = 100 + 100 * (blindCounter / 5)
    var whoFolded = 0

    var player1Stack = 10000
    var player2Stack = 10000

    val deck = Deck.full()
    deck.shuffle()
    val player1 = Hand.draw1(deck)
    val player2 = Hand.draw2(deck)
    val flopp = Hand.flopp(deck)
    val turn = Hand.turn(deck)
    val river = Hand.river(deck)

    var quit = false
    var continuePlaying = true

    def whoIsBlind: Int = {
        blindCounter % 2
    }

    def continuingBettingPhase: Unit = {
        (player2Bet > player1Bet) match {
            case true => {
                val input1 = scala.io.StdIn.readLine("Player 1's bet: ")
                if (input1 contains "Fold") {
                    quit = true 
                    whoFolded = 1
                }
                else {
                player1Bet += input1.toInt
                player1Stack -= player1Bet
                println(player1Bet)
                }
            }
            case false => 
        }
        (player2Bet < player1Bet) match {
            case true => {
                val input2 = scala.io.StdIn.readLine("Player 2's bet: ")
                if (input2 contains "Fold") {
                    quit = true 
                    whoFolded = -1
                }
                else {player2Bet += input2.toInt
                    player2Stack -= player2Bet 
                    println(player2Bet)
                    continuingBettingPhase
                }
            }
            case false => 
        }
    }

    def bettingPhase: Unit = {
        val input1 = scala.io.StdIn.readLine("Player 1's bet: ")
        if (input1 contains "Fold") {
            quit = true 
            whoFolded = 1
        }
        else {
            player1Bet += input1.toInt
            player1Stack -= player1Bet
            println(player1Bet)
        }
        if (!quit) {
            val input2 = scala.io.StdIn.readLine("Player 2's bet: ")
            if (input2 contains "Fold") {
                quit = true 
                whoFolded = -1
            }
            else {
                player2Bet += input2.toInt
                player2Stack -= player2Bet
                println(player2Bet)
            }
            if (!quit) {
                (player2Bet == player1Bet) match {
                    case true =>
                    case false => continuingBettingPhase
                }
            }
        }
    }

    def playOn: Unit = {
        val continueOrNot = scala.io.StdIn.readLine("Do you want to continue playing?\n YES or NO: ")
        if ((continueOrNot contains "YES") || (continueOrNot contains "Yes") || (continueOrNot contains "yes")) {
            continuePlaying = true
        }
        else if ((continueOrNot contains "NO") || (continueOrNot contains "No") || (continueOrNot contains "no")) {
            continuePlaying = false
        }
        else {
            continuePlaying = false
        }
    }

    def phaseOne: Unit = {
        println(s"\nThe players stacks are: \nPlayer 1: ${player1Stack}\nPlayer 2: ${player2Stack}")
        Thread.sleep(1000)
        println(s"\nThe blinds are ${smallBlind} and ${bigBlind}")
        Thread.sleep(1000)
        whoFolded = 0

        if (whoIsBlind == 1) {
            player1Stack -= bigBlind
            player2Stack -= smallBlind
            println(s"\nPlayer 1 is the big blind and Player 2 is the small blind")
        }
        else {
            player2Stack -= bigBlind
            player1Stack -= smallBlind
            println(s"Player 2 is the big blind and Player 1 is the small blind")
        }

        Thread.sleep(1000)

        println("\n\nPLAYER 2 TURN AWAY NOW")

        Thread.sleep(3000)
        
        println(s"\n\n\nPlayer 1's hand: \n\n${player1.cards(0)}, ${player1.cards(1)}")

        Thread.sleep(10000)

        println("\n\nTURN AWAY NOW")

        Thread.sleep(3000)

        println("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
        println(s"\nPlayer 2's hand: \n${player2.cards(0)}, ${player2.cards(1)}")

        Thread.sleep(10000)

        println("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")

        bettingPhase

        Thread.sleep(5000)

        println("\n\n\n\n")
    }

    def phaseTwo: Unit = {
        println(s"The flopp is : \n\n\n ${flopp.cards(0)}, ${flopp.cards(1)}, ${flopp.cards(2)}")

        Thread.sleep(10000)

        println(s"\n\n\nThe current pot is ${player1Bet + player2Bet + smallBlind + bigBlind}\n")

        bettingPhase

        println(s"The turn is : \n\n\n${flopp.cards(0)}, ${flopp.cards(1)}, ${flopp.cards(2)}, ${turn.cards(0)}")

        Thread.sleep(2000)

    }

    def phaseThree: Unit = {
        println(s"\n\n\nThe current pot is ${player1Bet + player2Bet + smallBlind + bigBlind}\n")

        bettingPhase

        println(s"\n\n\nThe river is : \n\n\n${flopp.cards(0)}, ${flopp.cards(1)}, ${flopp.cards(2)}, ${turn.cards(0)}, ${river.cards(0)}")
        
        Thread.sleep(2000)

        println(s"\n\n\nThe current pot is ${player1Bet + player2Bet + smallBlind + bigBlind}\n")

        bettingPhase

    }

    def phaseEnding: Unit = {
        val result = player1.isBetter(player1, player2)
        if (result == 1) {
            blindCounter += 1
            println(s"\n\n\nPlayer 1 won! They won ${player1Bet + player2Bet + smallBlind + bigBlind}")
            player1Stack += (player1Bet + player2Bet + smallBlind + bigBlind)

        }
        else if (result == -1) {
            blindCounter += 1
            println(s"\n\n\nPlayer 2 won! They won ${player1Bet + player2Bet + smallBlind + bigBlind}")
            player2Stack += (player1Bet + player2Bet + smallBlind + bigBlind)

        }
        else {
            if (whoIsBlind == 1) {
                blindCounter += 1
                println("\n\n\nIt was a tie!")
                player1Stack += player1Bet + bigBlind
                player2Stack += player2Bet + smallBlind
            }
            else {
                blindCounter += 1
                println("\n\n\nIt was a tie!")
                player1Stack += player1Bet + smallBlind
                player2Stack += player2Bet + bigBlind
            }
        }
        playOn
    }

    def gameLoop: Unit = {
        phaseOne       
        if (!quit) {
            phaseTwo 
            if (!quit) {    
                phaseThree
                if (!quit) {
                    phaseEnding
                }
            }
        }
        if (whoFolded == 1) {
            println("Player 1 folded!")
            player2Stack += (player1Bet + player2Bet + smallBlind + bigBlind)
        }
        else if (whoFolded == -1) {
            println("Player 2 folded!")
            player1Stack += (player1Bet + player2Bet + smallBlind + bigBlind)
        }
        playOn
    }

    def main(args: Array[String]): Unit = { 
        println("""
        |
        |First Player 1's hand will be shown for 10s, 
        |then Player 2's hand will be shown for 10s.
        |
        |After that the betting phase will begin!""".stripMargin)

        Thread.sleep(10000) 
        while (continuePlaying) {
            gameLoop
        } 
    }

}