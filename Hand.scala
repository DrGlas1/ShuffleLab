package cards

case class Hand(cards: Vector[Card]) {
  import Hand._

  /**
   * A vector of length 14 with positions 1-13 containing the number of
   * cards of that rank. Position 0 contains 0.
   */
  def tally: Vector[Int] = {
    val k = cards.length
    val tallyVector = Array.fill(14)(0)
    for (i <- 0 until k) {
      tallyVector(cards(i).rank) += 1
    }
    tallyVector.toVector
  }
  def checkKicker(hand: Hand): Int = {
    var highCard = 0
    var i = 0
    var quit = true
    while (i < 12 && quit) {
      hand.ranksSorted(i) match {
        case 1 => highCard = i; quit = false
        case _ => i += 1
      }
    }
    highCard
  }
  
  def compareKicker(player1: Hand, player2: Hand): Int = {
    if (checkKicker(player1) < checkKicker(player2)) 1
    else if (checkKicker(player1) > checkKicker(player2)) -1
    else 0
  }

  def isBetter(player1: Hand, player2: Hand): Int = {
    if (player1.category < player2.category) 1
    else if (player1.category > player2.category) -1
    else {
      player1.category match {
        case 0 => 0
        case 1 => {
          compareKicker(player1, player2)
        }
        case 2 => {
          compareKicker(player1, player2)
        }
        case 3 => 0
        case 4 => 0
        case 5 => 0
        case 6 => {
          compareKicker(player1, player2)
        }
        case 7 => {
          compareKicker(player1, player2)
        }
        case 8 => {
          compareKicker(player1, player2)
        }
        case 9 => {
          compareKicker(player1, player2)
        }
      }
    }
  }

  def ranksSorted: Vector[Int] = cards.map(_.rank).sorted.toVector

  def whatBullshitDidTheyGet(hand: Hand): String = {
    hand.category match {
      case 0 => "A ROYAL FLUSH!!! WHAT IN THE ABSOLUTE FUCK"
      case 1 => "A Straight Flush? Cool,cool,cool,cool,cool..."
      case 2 => "Four of a kind! Sick"
      case 3 => "A Full House!"
      case 4 => "A Flush!"
      case 5 => "A Straight"
      case 6 => "Threes"
      case 7 => "a Two Pair"
      case 8 => "a One Pair"
      case 9 => "Nothing..."
    }
  }

  def isFlush: Boolean = cards.length > 0 && cards.forall(_.suit == cards(0).suit)

  def isStraight: Boolean = {
    def isInSeq(xs: Vector[Int]): Boolean =
      xs.length > 1 && (0 to xs.length - 2).forall(i => xs(i) == xs(i + 1) - 1)

    isInSeq(ranksSorted) ||  // special case with ace interpreted as 14:
      (ranksSorted(0) == 1) && isInSeq(ranksSorted.drop(1) :+ 14)
  }
  def isRoyalStraight: Boolean = isStraight && tally(1) == 1 && tally(10) == 1 && tally(11) == 1 && tally(12) == 1 && tally(13) == 1
  def isStraightFlush: Boolean = isStraight && isFlush
  def isFour:          Boolean = tally.contains(4)
  def isFullHouse:     Boolean = tally.contains(3) && tally.contains(2)
  def isThrees:        Boolean = tally.contains(3)
  def isTwoPair:       Boolean = tally.count(_ == 2) == 2
  def isOnePair:       Boolean = tally.contains(2)

  def category: Int = // TODO: add more tests when tally is implemented
    if (isRoyalStraight && isFlush) Category.RoyalFlush
    else if (isStraight && isFlush) Category.StraightFlush
    else if (isFour)                Category.Fours
    else if (isFullHouse)           Category.FullHouse
    else if (isFlush)               Category.Flush
    else if (isStraight)            Category.Straight
    else if (isThrees)              Category.Threes
    else if (isTwoPair)             Category.TwoPair
    else if (isOnePair)             Category.OnePair
    else                            Category.HighCard
}
object Hand {
  def apply(cardSeq: Card*): Hand = new Hand(cardSeq.toVector)
  def from1(deck: Deck): Hand = new Hand(deck.peek(5))
  def from2(deck: Deck): Hand = new Hand(deck.peek2(5))
  def removeFrom(deck: Deck): Hand = new Hand(deck.remove(5))

  object Category {
    val RoyalFlush = 0
    val StraightFlush = 1
    val Fours = 2
    val FullHouse = 3
    val Flush = 4
    val Straight = 5
    val Threes = 6
    val TwoPair = 7
    val OnePair = 8
    val HighCard = 9
    val values = RoyalFlush to HighCard

    object Name {
      val english = Vector("royal flush", "straight flush", "four of a kind", "full house",
        "flush", "straight", "three of a kind", "two pairs", "pair", "high card")
      val swedish = Vector("royal flush", "färgstege", "fyrtal", "kåk", "färg",
        "stege", "tretal", "två par", "par", "högt kort")
    }
  }
}