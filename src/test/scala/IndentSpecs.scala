import com.codecommit.gll
import gll._
import gll.ast._

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalacheck._

object IndentSpecs extends Specification
  with NoTildeSyntax
  with ScalaCheck
  with IndentParsers {

  import Prop._

  "indenting" should {
    "handle no indentation" in {
      stm("while 1 2") must beLike {
        case Stream(Success(While(IntLit(1), List(IntLit(2))), LineStream())) => ok
      }
    }

    "handle simple indentation" in {
      stm("while 1\n 2") must beLike {
        case Stream(Success(While(IntLit(1), List(IntLit(2))), LineStream())) => ok
      }
    }

    "handle incorrect indentation" in {
      stm("while 1\n2") must beLike {
        case Stream(Failure(IndentError(_,_), _)) => ok
      }
    }

    "handle more incorrect indentation" in {
      stm(" while 1\n2") must beLike {
        case Stream(Failure(IndentError(_,_), _)) => ok
      }
    }

    "handle nested indentation" in {
      stm("""
while 1
  2
  while 3
    4
    5
  6
""") must beLike {
        case Stream(Success(While(IntLit(1), List(IntLit(2), While(IntLit(3), List(IntLit(4), IntLit(5))), IntLit(6))), LineStream())) => ok
      }
    }
  }

  // %%

  lazy val exp: Parser[Expr] = (
    num ^^ IntLit)

  lazy val stm: Parser[Expr] = (
    anchor(a => "while" ~ exp ~ indented(a)(block_+(stm))) ^^ { case (_, e, es) => While(e, es) } | exp)

  val num = """\d+""".r ^^ { _.toInt }

  // %%

  sealed trait Expr extends Node {
    def solve: Int
  }

  case class While(cond: Expr, body: List[Expr]) extends Expr {
    def solve = body.foldLeft(cond.solve) {
      case (left, e) => e.solve
    }

    def children = cond :: body

    def form = 'while ~ body.foldLeft[FormSpec](cond) {
      case (left, e) => left ~ e
    }
  }

  case class IntLit(i: Int) extends Expr with LeafNode {
    val solve = i
  }
}
