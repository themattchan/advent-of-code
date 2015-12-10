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

    def num = wholeNumber map Integer.parseInt

    def ON      : Parser[String] = "turn on"
    def OFF     : Parser[String] = "turn off"
    def TOGGLE  : Parser[String] = "toggle"
    def THROUGH : Parser[String] = "through"
    def COMMA   : Parser[String] = ","

    def parseCoord: Parser[Coord] =
      for {
        x <- num
        _ <- COMMA
        y <- num
      } yield (x,y)

    def parseCoordFromTo: Parser[(Coord,Coord)] =
      for {
        f <- parseCoord
        _ <- THROUGH
        t <- parseCoord
      } yield (f,t)

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
