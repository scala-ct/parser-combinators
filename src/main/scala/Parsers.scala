package parsing

trait Parsers {

  type Elem
  type Input = Reader[Elem]

  abstract class Parser[+T] extends (Input => ParseResult[T]) {

    def ~[U](that: => Parser[U]) = Parser[(T, U)] { in =>
      this(in) match {
        case f @ Failure(_) => f
        case Success(t, rest) => that(rest) match {
          case Success(u, rest2) => Success((t, u), rest2)
          case Failure(_) => Failure(in)
        }
      }
    }

    def |[U >: T](that: => Parser[U]) = Parser[U] { in =>
      this(in) match {
        case s @ Success(_, _) => s
        case Failure(_) => that(in)
      }
    }

  }

  /**
   * A ParseResult
   */
  abstract class ParseResult[+T]

  case class Success[+T](t: T, next: Input) extends ParseResult[T]
  case class Failure(next: Input) extends ParseResult[Nothing]


  def Parser[T](f: Input => ParseResult[T]): Parser[T] = new Parser[T] {
    def apply(in: Input) = f(in)
  }

  /**
   * some basic combinators
   */

  def acceptIf(p: Elem => Boolean) = Parser[Elem] { in =>
    if (!in.atEnd && p(in.first)) Success(in.first, in.rest)
    else Failure(in)
  }

  def accept(e: Elem) = acceptIf(_ == e)

}


abstract class Reader[+T] {

  type Position

  //def source:
  def pos: Position
  def first: T
  def rest: Reader[T]

  def atEnd: Boolean

}

case class CharArrayReader(val str: Array[Char], val pos: Int = 0) extends Reader[Char] {
  type Position = Int

  def first = str(pos)
  def rest = CharArrayReader(str, pos + 1)

  def atEnd = pos >= str.length

}

trait CharParsers extends Parsers {

  type Elem = Char
}


object HelloParser extends CharParsers {

  val a = accept('a')
  val in = CharArrayReader("aardvark".toArray)

  val aOrb = accept('a') | accept('b')
  val in2 = CharArrayReader("bossanova".toArray)

  def main(args: Array[String]) {
    println(a(in))
    println(a(in2))
    println(aOrb(in))
    println(aOrb(in2))
  }
}
