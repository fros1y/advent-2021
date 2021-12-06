// object Problem4 {
//   def input =
//     """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

// 22 13 17 11  0
//  8  2 23  4 24
// 21  9 14 16  7
//  6 10  3 18  5
//  1 12 20 15 19

//  3 15  0  2 22
//  9 18 13 17  5
// 19  8  7 25 23
// 20 11 10 24  4
// 14 21 16 12  6

// 14 21 17 24  4
// 10 16 15  9 19
// 18  8 23 26 20
// 22 11 13  6  5
//  2  0 12  3  7"""
//   extension [A](xs: Seq[A])
//     def splitWhen(p: A => Boolean): Seq[Seq[A]] = {
//       def go(xs: Seq[A], acc: Seq[Seq[A]]): Seq[Seq[A]] = xs match {
//         case Nil => acc
//         case x :: xs =>
//           if (p(x)) go(xs, acc :+ Nil)
//           else go(xs, acc.map(_ :+ x))
//       }
//       go(xs, Seq(Nil))
//     }

//   case class Card(rows: Seq[Set[Int]], cols: Seq[Set[Int]]) {
//     def winner(drawn: Seq[Int]): Boolean = {
//       val row = drawn.map(rows).reduce(_ & _)
//       val col = drawn.map(cols).reduce(_ & _)
//       row.size == 1 || col.size == 1
//     }

//     def numbers: Set[Int] = rows.flatten.toSet | cols.flatten.toSet
//   }

//   def parse_card(input: Seq[String]): Card = {
//     val rows = input.map(_.split(" ").map(_.toInt))
//     val cols = rows.transpose
//     return Card(rows.map(row => row.toSet), cols.map(col => col.toSet))
//   }

//   def parse(input: String): Tuple2[Seq[Int], Seq[Card]] = {
//     val lines = input.split("\n")
//     val drawn = lines.head.split(",").map(_.toInt)
//     val cards: Seq[Card] = lines.tail.splitWhen(_ == "").map(parse_card)
//     return (drawn, cards)
//   }

//   def find_winner(drawn: Seq[Int], cards: Seq[Card]): Option[Card] = {
//     val winners = cards.filter(_.winner(drawn))
//     if (winners.size == 1) Some(winners.head)
//     else None
//   }
// }
