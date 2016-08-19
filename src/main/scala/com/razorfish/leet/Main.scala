package com.razorfish.leet

import com.razorfish.leet.Leet.Leetable

object Leet {
  trait Leetable[T] {
    def toLeet(in: T): String
  }
  def apply[T](in: T)(implicit leetable: Leetable[T]): String =
    leetable.toLeet(in)
}

object MyLeet extends Leetable[String] {
  override def toLeet(in: String): String = {

    def replaceChars(x: Char): String = x match {
      case 'o' | 'O' => "0"
      case 'i' | 'I' => "1"
      case 'e' | 'E' => "3"
      case 'a' | 'A' => "4"
      case 's' | 'S' => "5"
      case 't' | 'T' => "7"

      case c => s"$c"
    }

    in flatMap {
      replaceChars
    }

  }
}

object Main {
  def main(args: Array[String]): Unit = {
    // add your implementation below this line
    implicit val implicVal = MyLeet

    println(args(0)+" -> "+Leet.apply(args(0)))

  }
}

