package slacc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {

    import ctx.reporter._

    val source = Source.fromFile(f)
    val keywords :Map [String,Token] = Map()

    // Complete this file

    new Iterator[Token] {

      var nextChar = source.next
      var pos = source.pos

      def hasNext = {
        source.hasNext
      }

      // will advance to the char after the parsed token, only calling
      // source.next to leave source at the next, useful for = vs ==
      def next = {
        if (nextChar.isLetter) {
          // string tokens, literals are taken in the symbols branch
          var strBuff = new StringBuffer

          while (nextChar.isLetter || nextChar.isDigit) {
            strBuff.append(nextChar)
            nextChar = source.next
          }
          // TODO Evaluate longest match and if all keywords are represented
          getStringToken(strBuff.toString).setPos(f, pos)
        } else if (nextChar.isDigit) {
          // digits
          var num = 0
          while (nextChar.isDigit) {
            num = 10*num + nextChar.asDigit
            nextChar = source.next
          }
          new INTLIT(num).setPos(f, pos)
        } else if (!nextChar.isWhitespace) {
          // symbol
          matchChar(nextChar)

        } else {
          nextChar = source.next
          pos = source.pos
          next
        }
      }
    }

  } // end of run

  // TODO Add the tokens we need to evaluate
  private def getStringToken(string: String): Token = string match {
//    case "object" => new Token(OBJECT)
    case "class" => new Token(CLASS)
    case "method" => new Token(METHOD)
//    case "def" => new Token(DEF)
    case "var" => new Token(VAR)
    case "unit" => new Token(UNIT)
//    case "main" => new Token(MAIN)
    case "string" => new Token(STRING)
    case "extends" => new Token(EXTENDS)
    case "int" => new Token(INT)
    case "boolean" => new Token(BOOLEAN)
    case "while" => new Token(WHILE)
    case "if" => new Token(IF)
    case "else" => new Token(ELSE)
    case "return" => new Token(RETURN)
    case "length" => new Token(LENGTH)
    case "true" => new Token(TRUE)
    case "false" => new Token(FALSE)
    case "self" => new Token(NEW)
    case "new" => new Token(NEW)
    case "println" => new Token(PRINTLN)
    case "strOf" => new Token(NEW)
    case "class" => new Token(CLASS)
    case _ => new ID(string)
  }

  private def matchChar(curr: Char) = char nextChar match {
    case '(' => {
      nextChar = source.next;
      pos = source.pos
      new Token(LPAREN).setPos(f, pos)
    }
    case ')' => {
      nextChar = source.next;
      pos = source.pos
      new Token(RPAREN).setPos(f, pos)
    }
    case '+' => {
      nextChar = source.next;
      pos = source.pos
      new Token(PLUS).setPos(f, pos)
    }
    // DIV is not that simple due to comments
    // TODO Add comment functionality
    case '/' => {
      nextChar = source.next;
      pos = source.pos

      if (nextChar == '/') {
        // skip the line
        while (nextChar != '\n') {
          nextChar = source.next;
          pos = source.pos
        }
        // found \n so move to the next
        nextChar = source.next;
        pos = source.pos
        next
      } else if (nextChar == '*') {
        var firChar = source.next
        var secChar = source.next
        while (firChar != '*' || secChar != '/') {
          firChar = secChar
          secChar = source.next
        }
        nextChar = source.next;
        pos = source.pos
        next
      } else {
        new Token(DIV).setPos(f, pos)
      }
    }
    case '*' => {
      nextChar = source.next;
      pos = source.pos
      new Token(TIMES).setPos(f, pos)
    }
    case '=' => {
      nextChar = source.next;
      pos = source.pos
      if (nextChar == '=') {
        nextChar = source.next;
        pos = source.pos
        new Token(EQUALS).setPos(f, pos)
      } else {
        new Token(EQSIGN).setPos(f, pos)
      }
    }
    case '<' => {
      nextChar = source.next;
      pos = source.pos
      if (nextChar == '=') {
        nextChar = source.next;
        pos = source.pos
        new Token(LESSTHANEQUALS).setPos(f, pos)
      } else {
        new Token(LESSTHAN).setPos(f, pos)
      }
    }
    case '>' => {
      nextChar = source.next;
      pos = source.pos
      if (nextChar == '=') {
        nextChar = source.next;
        pos = source.pos
        new Token(GREATERTHANEQUALS).setPos(f, pos)
      } else {
        new Token(GREATERTHAN).setPos(f, pos)
      }
    }
    case ':' => {
      nextChar = source.next
      pos = source.pos
      new Token(COLON).setPos(f, pos)
    }
    case ';' => {
      nextChar = source.next
      pos = source.pos
      new Token(SEMICOLON).setPos(f, pos)
    }
    case '.' => {
      nextChar = source.next
      pos = source.pos
      new Token(DOT).setPos(f, pos)
    }
    case ',' => {
      nextChar = source.next
      pos = source.pos
      new Token(COMMA).setPos(f, pos)
    }
    case '!' => {
      nextChar = source.next
      pos = source.pos
      new Token(BANG).setPos(f, pos)
    }
    case '[' => {
      nextChar = source.next
      pos = source.pos
      new Token(LBRACKET).setPos(f, pos)
    }
    case ']' => {
      nextChar = source.next
      pos = source.pos
      new Token(RBRACKET).setPos(f, pos)
    }
    case '{' => {
      nextChar = source.next
      pos = source.pos
      new Token(LBRACE).setPos(f, pos)
    }
    case '}' => {
      if (source.hasNext) {
        nextChar = source.next
        pos = source.pos
      }
      new Token(RBRACE).setPos(f, pos)
    }
    case '&' => {
      nextChar = source.next
      pos = source.pos
      if (nextChar == '&') {
        nextChar = source.next;
        pos = source.pos
        new Token(AND).setPos(f, pos)
      } else {
        new Token(BITWISEAND).setPos(f, pos)
      }
    }
    case '|' => {
      nextChar = source.next
      pos = source.pos
      if (nextChar == '|') {
        nextChar = source.next;
        pos = source.pos
        new Token(OR).setPos(f, pos)
      } else {
        new Token(BITWISEOR).setPos(f, pos)
      }
    }
    case '\"' => {
      nextChar = source.next
      pos = source.pos
      var strBuff = new StringBuffer
      while (nextChar != '\"') {
        strBuff.append(nextChar)
        nextChar = source.next
      }
      // pointing to final double quote, so need to progress one
      nextChar = source.next
      pos = source.pos
      new STRLIT(strBuff.toString).setPos(f, pos)
    }
  } // end of matchChar function

}



object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    new Iterator[Token] {
      def hasNext = {
        tokens.hasNext
      }

      def next = {
        val n = tokens.next
        println(n+"("+n.line+":"+n.col+") ")
        n
      }
    }
  }
}
