import java.io.FileReader
import scala.util.parsing.combinator._

object LightGrid {
  type Coord = (Int,Int)
  sealed abstract class Instr
  case class On     (tl: Coord, br: Coord) extends Instr
  case class Off    (tl: Coord, br: Coord) extends Instr
  case class Toggle (tl: Coord, br: Coord) extends Instr


  object ParseInstrs extends JavaTokenParsers {

    override def skipWhitespace = true
    def ON      : Parser[String] = "turn on"
    def OFF     : Parser[String] = "turn off"
    def TOGGLE  : Parser[String] = "toggle"
    def THROUGH : Parser[String] = "through"
    def COMMA   : Parser[String] = ","

    def num = wholeNumber map Integer.parseInt

    def parseCoord: Parser[Coord] =
      for {
        x <- num
        _ <- COMMA
        y <- num
      } yield (x,y)

    def parseCoordFromTo: Parser[(Coord,Coord)] =
      for {
        from <- parseCoord
        _ <- THROUGH
        to <- parseCoord
      } yield (from,to)

    def parseOn : Parser[Instr] = ON     ~> parseCoordFromTo ^^ { case (f,t) => On(f,t) }
    def parseOff: Parser[Instr] = OFF    ~> parseCoordFromTo ^^ { case (f,t) => Off(f,t) }
    def parseTog: Parser[Instr] = TOGGLE ~> parseCoordFromTo ^^ { case (f,t) => Toggle(f,t) }

    def parseInstrs = (parseOn | parseOff | parseTog)*
  }

  def main(args: Array[String]) = {
    val input = new FileReader("input.txt")
    val ins = ParseInstrs
      .parseAll(ParseInstrs.parseInstrs, input)
      .getOrElse(List())

    println(ins)
    input.close
  }

}

/*
 object Wrap extends Dim {

 def surface(x: List[Int]) = {
    val y = x ++ x
    val b = y.zip(y.tail).take(x.length)
    val c = b map { case (x,y) => x*y } reduce (_+_)
    c*2
  }

  def slack(x: List[Int]) = x match {
    case a::b::_ => a*b
    case _ => throw new Exception
  }

  def wrapping(x: List[Int]) = surface(x) + slack(x)

  def volume(x: List[Int]) = x.reduce(_*_)

  def peri(x: List[Int]) = x match {
    case a::b::_ => (a+b)*2
    case _ => throw new Exception
  }

  def ribbon(x: List[Int]) = volume(x) + peri(x)

  def main(args: Array[String]) = {
    val input = new FileReader("input.txt")
    val dims = parseAll(dim, input).getOrElse(List())

    val totWrap = dims.map(wrapping).reduce(_+_)
    println(totWrap)

    val totRibbon = dims.map(ribbon).reduce(_+_)
    println(totRibbon)

    input.close
  }
}
 */
