import java.io.FileReader
import scala.util.parsing.combinator._

class Dim extends JavaTokenParsers {

  override def skipWhitespace = true

  def num = wholeNumber map Integer.parseInt
  def sep = literal("x")

  def dim1 = for {
    l <- num
    _ <- sep
    w <- num
    _ <- sep
    h <- num
  } yield List(l,w,h).sorted

  def dim = rep(dim1)
}

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
