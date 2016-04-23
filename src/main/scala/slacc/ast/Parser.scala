package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)
    var mainMethodFound: Boolean = false

    def readToken: Unit = {
      if (tokens.hasNext) {

        // uses nextToken from the Lexer trait
        currentToken = tokens.next
        println(currentToken.toString)
        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected.
    The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") +
        ", found: " + currentToken, currentToken)
    }

    def parseFormal(): Formal = {
      if (currentToken.kind == IDKIND) {
        val id = currentToken.asInstanceOf[ID].value
        readToken
        eat(COLON)
        currentToken.kind match {
          case INT => {
            readToken
            currentToken.kind match {
              case LBRACKET => {
                readToken
                eat(RBRACKET)
                new Formal(IntArrayType(), Identifier(id))
              }
              case _ => {
                new Formal(IntType(), Identifier(id))
              }
            }
          }
          case BOOLEAN => {
            readToken
            new Formal(BooleanType(), Identifier(id))
          }
          case STRING => {
            readToken
            new Formal(StringType(), Identifier(id))
          }
          case UNIT => {
            readToken
            new Formal(UnitType(), Identifier(id))
          }
          case _ => {
            if (currentToken.kind == IDKIND) {
              val userDefinedFormal = new Formal(UserDefinedType(Identifier(currentToken.asInstanceOf[ID].value)),
                Identifier(id))
              readToken
              userDefinedFormal
            }
            else {
              expected(IDKIND)
            }
          }
        }
      }
      else {
        expected(IDKIND)
      }
    }



    def parseMethodReturnType(): TypeTree = {
      eat(RPAREN)
      eat(COLON)

      currentToken.kind match {
        case INT => {
          readToken
          currentToken.kind match {
            case LBRACKET => {
              readToken
              eat(RBRACKET)
              eat(EQSIGN)
              new IntArrayType()
            }
            case _ => {
              eat(EQSIGN)
              new IntType()
            }
          }
        }
        case BOOLEAN => {
          readToken
          eat(EQSIGN)
          new BooleanType()
        }
        case STRING => {
          readToken
          eat(EQSIGN)
          new StringType()
        }
        case UNIT => {
          readToken
          eat(EQSIGN)
          new UnitType()
        }
        case _ => {
          if (currentToken.kind == IDKIND) {
            val userDefinedType = new UserDefinedType(Identifier(currentToken.asInstanceOf[ID].value))
            readToken
            eat(EQSIGN)
            userDefinedType
          }
          else {
            expected(IDKIND)
          }
        }
      }
    }

    def parseVarDecl(): VarDecl = {
      // var Identifier : Type ;
      eat(VAR)
      if (currentToken.kind == IDKIND) {
        val id = currentToken.asInstanceOf[ID].value
        readToken
        eat(COLON)

        currentToken.kind match {
          case INT => {
            readToken
            currentToken.kind match {
              case LBRACKET => {
                readToken
                eat(RBRACKET)
                eat(SEMICOLON)
                new VarDecl(IntArrayType(), Identifier(id))
              }
              case _ => {
                eat(SEMICOLON)
                new VarDecl(IntType(), Identifier(id))
              }
            }
          }
          case BOOLEAN => {
            eat(SEMICOLON)
            new VarDecl(BooleanType(), Identifier(id))
          }
          case STRING => {
            eat(SEMICOLON)
            new VarDecl(StringType(), Identifier(id))
          }
          case UNIT => {
            eat(SEMICOLON)
            new VarDecl(UnitType(), Identifier(id))
          }
          case _ => {
            if (currentToken.kind == IDKIND) {
              val userDefinedVar = new VarDecl(UserDefinedType(Identifier(currentToken.asInstanceOf[ID].value)),
                Identifier(id))
              eat(SEMICOLON)
              userDefinedVar
            }
            else {
              fatal("Unknown Kind")
            }
          }
        }
      }
      else {
        expected(IDKIND)
      }
    }

    def parseExprTree(): ExprTree = {

      currentToken.kind match {
        case TRUE => {
          readToken
          True()
        }
        case FALSE => {
          readToken
          False()
        }
        case _ => {
          fatal("Unknow Expressions.")
        }
      }
    }

    def parseMethodBody(): (List[VarDecl], List[ExprTree], ExprTree) = {
      eat(LBRACE)
      // parse the body
      var varList = List[VarDecl]();
      while (currentToken.kind == VAR) {
        varList :+= parseVarDecl
      }

      var exprList = List[ExprTree]()
      exprList :+= parseExprTree
      while(currentToken.kind == SEMICOLON) {
        readToken
        exprList :+= parseExprTree
      }


      eat(RBRACE)
      (varList, exprList, True())
    }

    def parseMethodDecl(id: ID): MethodDecl = {
      val methodId = new Identifier(id.value)
      eat(IDKIND)
      eat(LPAREN)

      currentToken.kind match {
        case IDKIND => {
          var formalList = List[Formal]()
          formalList :+= parseFormal()
          while (currentToken.kind == COMMA) {
            readToken
            formalList :+= parseFormal()
          }
          val retType = parseMethodReturnType()
          val (varList, exprList, exprTree) = parseMethodBody()
          new MethodDecl(retType, methodId, formalList, varList, exprList , exprTree)
        }

        case RPAREN => {
          val retType = parseMethodReturnType()
          val (varList, exprList, exprTree) = parseMethodBody()
          new MethodDecl(retType, methodId, List[Formal](), varList, exprList , exprTree)
        }
        case _ => {
          ctx.reporter.fatal("Expecting arguments or right parenthesis.")
        }
      }
    }

    def parseClass(id: ID): ClassDecl = {
      readToken

      val classId = new Identifier(id.value)

      // TODO: parent class
      var varList = List[VarDecl]()
      readToken
      while (currentToken.kind == VAR) {
        val currVarDecl = parseVarDecl()

        readToken
      }

      var methodList = List[MethodDecl]()

      readToken
      while (currentToken.kind == METHOD) {
        eat(METHOD)
        currentToken match {
          case idToken: ID => {
            parseMethodDecl(idToken)
            readToken
          }
          case _ => throw new ClassCastException
        }
      }

      new ClassDecl(classId, None, varList, methodList)
    }

    def parseGoal(): Program = {
      while (currentToken.kind == CLASS) {
        eat(CLASS)
        currentToken match {
          case idToken: ID => {
            parseClass(idToken)
            readToken
          }
          case _ => throw new ClassCastException
        }
      }
      if (currentToken.kind == METHOD) {
        if (mainMethodFound) {
          ctx.reporter.fatal("Exactly one MainMethod declared after class declarations. At least 2 found.")
        }
        else {
          mainMethodFound = true
          eat(METHOD)
          currentToken match {
            case idToken: ID => {
              val mainMethod = new MainMethod(parseMethodDecl(idToken))
              Program(mainMethod, List[ClassDecl]())
            }
            case _ => throw new ClassCastException
          }
        }
      }
      else {
        // Errors
        if (!mainMethodFound) {
          ctx.reporter.fatal("Exactly one MainMethod declared after class declarations. 0 found.")
        }
        ctx.reporter.fatal("Unexpected Token")
      }
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  } // end of run
} // end of object parser
