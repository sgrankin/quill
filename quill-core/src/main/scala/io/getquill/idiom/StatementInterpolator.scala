package io.getquill.idiom

import io.getquill.ast._
import io.getquill.util.Interleave
import io.getquill.util.Messages._

import scala.collection.mutable

object StatementInterpolator {

  trait Tokenizer[T] {
    def token(v: T): Token
  }

  object Tokenizer {
    def apply[T](f: T => Token) = new Tokenizer[T] {
      def token(v: T) = f(v)
    }
  }

  implicit class TokenImplicit[T](v: T)(implicit tokenizer: Tokenizer[T]) {
    def token = tokenizer.token(v)
  }

  implicit def stringTokenizer: Tokenizer[String] =
    Tokenizer[String] {
      case string => StringToken(string)
    }

  implicit def liftTokenizer: Tokenizer[Lift] =
    Tokenizer[Lift] {
      case lift: ScalarLift => ScalarLiftToken(lift)
      case lift             => fail(s"Can't tokenize a non-scalar lifting. ${lift.name}")
    }

  implicit def tokenTokenizer: Tokenizer[Token] = Tokenizer[Token](identity)
  implicit def statementTokenizer: Tokenizer[Statement] = Tokenizer[Statement](identity)
  implicit def stringTokenTokenizer: Tokenizer[StringToken] = Tokenizer[StringToken](identity)
  implicit def liftingTokenTokenizer: Tokenizer[ScalarLiftToken] = Tokenizer[ScalarLiftToken](identity)

  implicit class TokenList[T](list: List[T])(implicit tokenize: Tokenizer[T]) {
    def mkStmt(sep: String = ", ")(implicit tokenize: Tokenizer[T]) = {
      val l1 = list.map(_.token)
      val l2 = List.fill(l1.size - 1)(StringToken(sep))
      Statement(Interleave(l1, l2))
    }
  }

  implicit def listTokenizer[T](implicit tokenize: Tokenizer[T]): Tokenizer[List[T]] =
    Tokenizer[List[T]] {
      case list => list.mkStmt()
    }

  implicit class Impl(sc: StringContext) {

    private def flatten(tokens: mutable.ListBuffer[Token]) = {
      val output = mutable.ListBuffer.empty[Token]
      while (tokens.nonEmpty) {
        tokens.remove(0) match {
          case Statement(list) =>
            tokens.prependAll(list)

          case StringToken(s) =>
            val strings = new mutable.StringBuilder(s)
            while (tokens.nonEmpty && tokens.head.isInstanceOf[StringToken]) {
              strings ++= tokens.remove(0).asInstanceOf[StringToken].string
            }
            val string = strings.result()
            if (string.nonEmpty)
              output += StringToken(string)

          case token =>
            output += token
        }
      }
      output
    }

    def stmt(args: Token*): Statement = {
      sc.checkLengths(args)
      val partsIterator = sc.parts.iterator
      val argsIterator = args.iterator
      val tokens = mutable.ListBuffer.empty[Token]
      tokens += StringToken(partsIterator.next())
      while (argsIterator.hasNext) {
        tokens += argsIterator.next
        tokens += StringToken(partsIterator.next())
      }
      Statement(flatten(tokens).result())
    }
  }
}
