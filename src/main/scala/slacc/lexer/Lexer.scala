package slacc
package lexer

import java.io.File

import slacc.utils._

import scala.io.Source

object Lexer extends Pipeline[File, Iterator[Token]] {

  import Tokens._

  var currChar: Option[Char] = None // the actual char that the lexer is at
  var peekChar: Option[Char] = None // used to peek next char
  var pos: Int = 0

  def run(ctx: Context)(f: File): Iterator[Token] = {

    val source = Source.fromFile(f)
    var eof: Boolean = false

    pos = source.pos

    // Complete this file
    new Iterator[Token] {

      def hasNext = {
        if (source.hasNext) {
          true
        }
        else if (eof) {
          false
        }
        else {
          eof = true
          true
        }
      }

      def next = {
        if (eof) {
          pos = source.pos
          new Token(EOF).setPos(f, pos)
        }
        else {
          if (currChar.isEmpty) {
            currChar = Some(source.next)
          }
          // try to skip white space and empty lines first
          while((currChar.get.isWhitespace || currChar.get == '\n') && !eof) {
            if (source.hasNext) {
              currChar = Some(source.next)
            } else {
              eof = true
            }
          }

          pos = source.pos
          if (eof) {
            new Token(EOF).setPos(f, pos)
          } else {
            matchInitialChar(source, f)
          }
        }
      } // end of next method of iterator
    } // end of Iterator[Token]
  } // end of run

  private def matchInitialChar(source: Source, f: File): Token = {
    if (currChar.get.isLetter) {
      var strBuff = new StringBuffer
      strBuff.append(currChar.get)

      if (source.hasNext) {
        peekChar = Some(source.next)
      }

      // apply longest match rule
      while (
        !peekChar.isEmpty
        &&
        !peekChar.get.isSpaceChar
        &&
        (peekChar.get.isLetter || peekChar.get.isDigit|| peekChar.get == '_')
        )
      {
        strBuff.append(peekChar.get)
        if (source.hasNext) {
          peekChar = Some(source.next)
        }
        else {
          peekChar = None
        }
      }

      currChar = peekChar

      val collectedStr = strBuff.toString
      val token = getStringToken(collectedStr)
      token.setPos(f, pos)
      token
    } // end of case letter
    else if (currChar.get.isDigit) {
      // digits
      var num = 0
      while (currChar.get.isDigit) {
        num = 10*num + currChar.get.asDigit
        currChar = Some(source.next)
      }
      new INTLIT(num).setPos(f, pos)
    } // end of case digit
    else if (!currChar.get.isWhitespace) {
      matchSymbol(source, f)
    } // end of case whitespace
    else { // only thing left is the bad things
      println("char \'" + currChar.get + "\' makes it bad.")
        new Token(BAD).setPos(f,pos)
    }
  }

  private def matchSymbol(source: Source, f: File): Token = {
    val cc = currChar.get
    cc match {
      case '(' | ')' | '+' | '-' | '*' | ':' | ';' | '.' | ','| '!' | '[' | ']' | '{' |'}' => {
        resetCurrCharAndReturnToken(cc, f)
      }

      case '=' => {
        peekChar = peek(source)
        if (peekChar.get == '=') {
          currChar = None
          new Token(EQUALS).setPos(f, pos)
        } else {
          currChar = peekChar
          new Token(EQSIGN).setPos(f, pos)
        }
      }
      case '<' => {
        peekChar = peek(source)
        if (peekChar.get == '=') {
          currChar = None
          new Token(LESSTHANEQUALS).setPos(f, pos)
        } else {
          currChar = peekChar
          new Token(LESSTHAN).setPos(f, pos)
        }
      }
      case '>' => {
        peekChar = peek(source)
        if (peekChar.get == '=') {
          currChar = None
          new Token(GREATERTHANEQUALS).setPos(f, pos)
        } else {
          currChar = peekChar
          new Token(GREATERTHAN).setPos(f, pos)
        }
      }
      case '&' => {
        peekChar = peek(source)
        if (peekChar.get == '&') {
          currChar = None
          new Token(AND).setPos(f, pos)
        } else {
          currChar = peekChar
          new Token(BITWISEAND).setPos(f, pos)
        }
      }
      case '|' => {
        peekChar = peek(source)
        if (peekChar.get == '|') {
          currChar = None
          new Token(OR).setPos(f, pos)
        } else {
          currChar = peekChar
          new Token(BITWISEOR).setPos(f, pos)
        }
      }
      case '\"' => {
        var strBuff = new StringBuffer

        peekChar = peek(source)
        while (peekChar.get != '\"') {
          strBuff.append(peekChar.get)
          peekChar = Some(source.next)
        }
        currChar = None
        // pointing to final double quote, so need to progress one
        new STRLIT(strBuff.toString).setPos(f, pos)
      }

      // div and comment line and comment block
      case '/' => {
        peekChar = peek(source)
        if (peekChar.get == '/') {

          val tk = new Token(CMT).setPos(f, pos)
          // skip the line
          while (peekChar.get != '\n') {
            peekChar = peek(source)
          }
          currChar = None
          // found \n so move to the next
          tk
        }
        else if (peekChar.get == '*') {
          val tk = new Token(CMT).setPos(f, pos)
          var firChar = source.next
          var secChar = source.next

          while (firChar != '*' || secChar != '/') {
            firChar = secChar
            secChar = source.next
          }
          currChar = None;
          tk
        } else {
          currChar = peekChar
          new Token(DIV).setPos(f, pos)
        }
      }
      case _ => new Token(BAD).setPos(f,pos)
    } // end of matchSymbol
  } // end of matchChar function

  private def resetCurrCharAndReturnToken(c: Char, f: File): Token = {
    currChar = None

    c match {
      case '(' => new Token(LPAREN).setPos(f, pos)
      case ')' => new Token(RPAREN).setPos(f, pos)
      case '+' => new Token(PLUS).setPos(f, pos)
      case '-' => new Token(MINUS).setPos(f, pos)
      case '*' => new Token(TIMES).setPos(f, pos)
      case ':' => new Token(COLON).setPos(f, pos)
      case ';' => new Token(SEMICOLON).setPos(f, pos)
      case '.' => new Token(DOT).setPos(f, pos)
      case ',' => new Token(COMMA).setPos(f, pos)
      case '!' => new Token(BANG).setPos(f, pos)
      case '[' => new Token(LBRACKET).setPos(f, pos)
      case ']' => new Token(RBRACKET).setPos(f, pos)
      case '{' => new Token(LBRACE).setPos(f, pos)
      case '}' => new Token(RBRACE).setPos(f, pos)
      case _ => new Token(BAD).setPos(f,pos)
    }
  }

  private def peek(s: Source): Option[Char] = {
    var peekChar: Option[Char] = None
    if (s.hasNext) {
      peekChar = Some(s.next)
    }
    peekChar
  }

  private def getStringToken(string: String): Token = string match {
    case "class" => new Token(CLASS)
    case "method" => new Token(METHOD)
    case "var" => new Token(VAR)
    case "Unit" => new Token(UNIT)
    case "String" => new Token(STRING)
    case "extends" => new Token(EXTENDS)
    case "Int" => new Token(INT)
    case "Bool" => new Token(BOOLEAN)
    case "while" => new Token(WHILE)
    case "if" => new Token(IF)
    case "else" => new Token(ELSE)
    case "return" => new Token(RETURN)
    case "length" => new Token(LENGTH)
    case "true" => new Token(TRUE)
    case "false" => new Token(FALSE)
    case "self" => new Token(SELF)
    case "new" => new Token(NEW)
    case "println" => new Token(PRINTLN)
    case "strOf" => new Token(STROF)
    case _ => new ID(string)
  }
} // end of object Lexer

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
    } // end of new Iterator[Token]
  } // end of run
}  // end of object DisplayTokens
