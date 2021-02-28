package com.evobootcamp.homeworks.adt

object AlgebraicDataTypes {
  type ErrorMessage = String
  type Weight = Int
  type ID = Int
  type Cards = List[Card]
  type Hands = List[Hand]
  type TestResults = List[TestResultItem]

  sealed trait Suit
  object Suit {
    final case object Hearth extends Suit
    final case object Diamond extends Suit
    final case object Club extends Suit
    final case object Spade extends Suit
  }

  sealed trait Rank
  object Rank {
    final case object Two extends Rank
    final case object Three extends Rank
    final case object Four extends Rank
    final case object Five extends Rank
    final case object Six extends Rank
    final case object Seven extends Rank
    final case object Eight extends Rank
    final case object Nine extends Rank
    final case object Ten extends Rank
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
    final case object Ace extends Rank
  }

  sealed trait Game
  object Game {
    final case object Texas extends Game
    final case object Omaha extends Game
  }

  final case class Card(suit: Suit, rank: Rank)

  sealed trait CardSet {
    def cards: Cards
  }

  sealed abstract case class Hand private (id: ID, cards: Cards) extends CardSet
  object Hand {
    def create(game: Game, id: ID, cards: Cards): Either[ErrorMessage, Hand] = game match {
      case Game.Texas => if (cards.length == 2) Right(new Hand(id, cards) {}) else Left("Number of cards should be 2")
      case Game.Omaha => if (cards.length == 4) Right(new Hand(id, cards) {}) else Left("Number of cards should be 4")
      case _  => Left("Unsupported game")
    }
  }

  sealed abstract case class Board private (cards: Cards) extends CardSet
  object Board {
    def create(cards: Cards): Either[ErrorMessage, Board] =
      if (cards.length == 5) Right(new Board(cards) {}) else Left("Number of cards should be 5")
  }

  sealed trait Combination
  object Combination {
    final case object StraightFlush extends Rank
    final case object FourKind extends Rank
    final case object FullHouse extends Rank
    final case object Flush extends Rank
    final case object Straight extends Rank
    final case object StraightLower extends Rank
    final case object ThreeKind extends Rank
    final case object TwoPairs extends Rank
    final case object Pair extends Rank
    final case object HighCard extends Rank
  }

  final case class CombinationResult(combination: Combination, handCards: Cards, boardCards: Cards)

  final case class TestResultItem(hand: Hand, combination: CombinationResult, weight: Weight)

  final case class TestResult(items: TestResults)

  final case class TestCase(game: Game, board: Board, hands: Hands) {
    def getResult(): TestResult = ???
  }
}
