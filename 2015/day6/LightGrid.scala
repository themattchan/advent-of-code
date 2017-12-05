import java.io.FileReader
import scala.util.parsing.combinator._

object LightGrid {
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

  type Coord = (Int,Int)
  sealed trait Instr
  case class On     (tl: Coord, br: Coord) extends Instr
  case class Off    (tl: Coord, br: Coord) extends Instr
  case class Toggle (tl: Coord, br: Coord) extends Instr

  def inBounds (tl : Coord, br : Coord) : (Coord => Boolean) = {
    val (tlx,tly) = tl
    val (brx,bry) = br

    { case (x,y) => (tlx <= x && x <= brx) && (tly <= y && y <= bry) }
  }

  val points =
    for { x <- 0 to 999
          y <- 0 to 999
        } yield (x,y)

  def solve1 (commands : List[Instr]) : Int = {
    def applyCommands (pt : Coord, st0 : Boolean) = {
      commands.foldLeft (st0) { (st, comm) =>
        comm match {
          case On      (tl, br) if inBounds (tl, br) (pt) => true
          case Off     (tl, br) if inBounds (tl, br) (pt) => false
          case Toggle  (tl, br) if inBounds (tl, br) (pt) => ! st
          case _ => st
        }
      }
    }

    points.foldLeft (0) { (s,pt) => if (applyCommands (pt, false)) s+1 else s }
  }

  def solve2 (commands : List[Instr]) : Int = {
    def applyCommands (pt : Coord, st0 : Int) = {
      commands.foldLeft (st0) { (st, comm) =>
        comm match {
          case On      (tl, br) if inBounds (tl, br) (pt) => st + 1
          case Off     (tl, br) if inBounds (tl, br) (pt) => 0 max (st - 1)
          case Toggle  (tl, br) if inBounds (tl, br) (pt) => st + 2
          case _ => st
        }
      }
    }

    points.foldLeft (0) { (b, pt) => applyCommands(pt, 0) + b }
  }

  def bracketFile[A](file : String)(f : FileReader => A) : Option[A] = {
    try {
      val input = new FileReader(file)
      val ret = Some (f (input))

      input.close
      ret
    } catch { case e: Exception => {
        println ("uh oh")
        None
      }
    }
  }

  def main(args: Array[String]) = {

    val ins = bracketFile("input.txt")
                { ParseInstrs
                    .parseAll(ParseInstrs.parseInstrs, _)
                    .getOrElse(List())
                }

    ins foreach (solve1 _ andThen println)
    ins foreach (solve2 _ andThen println)
  }

}
