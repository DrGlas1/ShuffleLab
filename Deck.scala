package cards

class Deck private (val initCards: Vector[Card]){
  private var cards: Array[Card] = initCards.toArray

  def reset(): Unit = cards = initCards.toArray
  def apply(i: Int): Card = cards(i)
  def toVector: Vector[Card] = cards.toVector
  def drop(n: Int): Vector[Card] = cards.drop(n).toVector 
  override def toString: String = cards.mkString(" ")

  def peek(n: Int): Vector[Card] = cards.take(n).toVector
  def peek2(n: Int): Vector[Card] = cards.drop(n).take(n).toVector

  def chooseFlopp: Vector[Card] = cards.drop(4).take(3).toVector
  def chooseTurn: Vector[Card] = cards.drop(7).take(1).toVector
  def chooseRiver: Vector[Card] = cards.drop(8).take(1).toVector

  def remove(n: Int): Vector[Card] = {
    val init = peek(n)
    cards = cards.drop(n)
    init
  }

  def prepend(moreCards: Card*): Unit = cards = moreCards.toArray ++ cards

  /** Swaps cards at position a and b. */
  def swap(a: Int, b: Int): Unit = {
      val placeholder = cards(a)
      cards(a) = cards(b)
      cards(b) = placeholder
  }

  /** Randomly reorders the cards in this deck. */
  def shuffle(): Unit = {
      for (i <- cards.length - 1 to 0 by -1) {
          val r = util.Random.nextInt(i + 1)
          swap(i, r)
      }
  }
}

object Deck {
  def empty: Deck = new Deck(Vector())
  def apply(cards: Seq[Card]): Deck = new Deck(cards.toVector)

  /** Creates a new full Deck with 52 cards in rank and suit order. */
  def full(): Deck = {
      val newDeck = Array.ofDim[Card](52)
      var x = 0
      for (i <- Card.suitRange; j <- Card.rankRange) {
          newDeck(x) = Card(j, i)
          x += 1
      }
    apply(newDeck)
  }
}