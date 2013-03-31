package com.codecommit.gll

trait IndentParsers extends RegexParsers {
  protected override val skipWhitespace = true

  case class Anchor(line: Int, col: Int)
  val dummyAnchor = Anchor(0,0)

  def anchor[R](p: Anchor => Parser[R]): Parser[R] = {
    new NonTerminalParser[R] {
      override def isPreferred = p(dummyAnchor).isPreferred

      def computeFirst(seen: Set[Parser[Any]]) = p(dummyAnchor).computeFirst(seen + this)

      def chain(t: Trampoline, in: LineStream)(f2: Result[R] => Unit) {
        val in2 = handleWhitespace(in)
//        println("anchor -> " + (in2.lineNum, in2.colNum))
        p(Anchor(in2.lineNum, in2.colNum)).chain(t, in) {
          result =>
            f2(result)
        }
      }
    }
  }

  protected def indentFilter[R](op: (Anchor, Anchor) => Boolean)(a: Anchor)(p: => Parser[R]): Parser[R] = {
    new NonTerminalParser[R] {
      override def isPreferred = p.isPreferred

      def computeFirst(seen: Set[Parser[Any]]) = p.computeFirst(seen + this)

      def chain(t: Trampoline, in: LineStream)(f2: Result[R] => Unit) {
        val gobble = handleWhitespace(in)
        println("compare " + Anchor(gobble.lineNum, gobble.colNum) + " vs. " + a)
        if (op(Anchor(gobble.lineNum, gobble.colNum), a))
          p.chain(t, in)(f2)
        else
          f2(Failure(IndentError((a.line, a.col), (in.lineNum, in.colNum)), in))
      }
    }
  }

  def newline: Parser[Unit] = ("\n" | "\r\n" | "\r") ^^ { case _ => () }

  // A^>
  def indented[T] = indentFilter[T]((pos, a) => pos.col > a.col) _
  // A^>=
  def maybeIndented[T] = indentFilter[T]((pos, a) => pos.col >= a.col) _
  // A^=
  def sameIndent[T] = indentFilter[T]((pos, a) => pos.col == a.col) _

  def sameLine[T] = indentFilter[T]((pos, a) => pos.line == a.line) _
  def differentLine[T] = indentFilter[T]((pos, a) => pos.col > a.col) _
  // ensure must skip a line -- better done with newline token, though
  def skipLine[T] = indentFilter[T]((pos, a) => pos.line > a.line + 1) _

  def block_+[T](p: => Parser[T]): Parser[List[T]] = anchor(a => sameIndent(a)(p)+)
  def block_*[T](p: => Parser[T]): Parser[List[T]] = anchor(a => sameIndent(a)(p)*)
  // for consistency?
  def block_?[T](p: => Parser[T]): Parser[Option[T]] = (p?)
}

object IndentParsers extends IndentParsers