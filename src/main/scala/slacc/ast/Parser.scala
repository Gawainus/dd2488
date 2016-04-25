package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  val firstOfFactor = List[TokenKind](TIMES, DIV)
  val firstOfTerm = List[TokenKind](PLUS, MINUS)

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
      * The method accepts arbitrarily many arguments of type TokenKind */
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
            readToken
            eat(SEMICOLON)
            new VarDecl(BooleanType(), Identifier(id))
          }
          case STRING => {
            readToken
            eat(SEMICOLON)
            new VarDecl(StringType(), Identifier(id))
          }
          case UNIT => {
            readToken
            eat(SEMICOLON)
            new VarDecl(UnitType(), Identifier(id))
          }
          case _ => {
            if (currentToken.kind == IDKIND) {
              val userDefinedVar = new VarDecl(UserDefinedType(Identifier(currentToken.asInstanceOf[ID].value)),
                Identifier(id))
              readToken
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

    // math part
    def parseFactor(): ExprTree = {
      currentToken.kind match {
        case IDKIND => {
          val idStr = currentToken.asInstanceOf[ID].value
          readToken
          Identifier(idStr)
        }
        case INTLITKIND => {
          val intValue = currentToken.asInstanceOf[INTLIT].value
          readToken
          new IntLit(intValue)
        }
        case LPAREN => {
          parseParenExprTree
        }
        case _ => {
          val firstExpr = parseExprTree()
          currentToken.kind match {
            case LBRACKET => {
              readToken
              val index = parseExprTree
              ArrayRead(firstExpr, index)
            }
            case DOT => {
              readToken
              currentToken.kind match {
                case LENGTH => {
                  ArrayLength(firstExpr)
                }
                case IDKIND => {
                  val idStr = currentToken.asInstanceOf[ID].value
                  val params = parseParams
                  MethodCall(firstExpr, Identifier(idStr), params)
                }
                case _ => {
                  fatal("Not a valid DOT operation")
                }
              }
            }
          }
        }
      }
    }

    def parseFactorList(firstExpr: ExprTree): ExprTree = {
      var currFactor = firstExpr
      while (currentToken.kind == TIMES | currentToken.kind == DIV) {
        currentToken.kind match {
          case TIMES => {
            readToken
            currFactor = Times(currFactor, parseFactor)
          }
          case _ => {
            readToken
            currFactor = Div(currFactor, parseFactor)
          }
        }
      }
      currFactor
    }

    def parseTerm(lhs: ExprTree): ExprTree = {
      currentToken.kind match {
        case PLUS => {
          readToken
          val firstExpr = parseFactor()
          val rhs = parseFactorList(firstExpr)
          Plus(lhs, rhs)
        }

        case MINUS => {
          readToken
          val firstExpr = parseFactor()
          val rhs = parseFactorList(firstExpr)
          Minus(lhs, rhs)
        }

        case _ => {
          fatal("Not a valid term.")
        }
      }
    }

    def parseTermList(firstExpr: ExprTree): ExprTree = {
      var currTerm = parseFactorList(firstExpr)
      while (currentToken.kind == PLUS | currentToken.kind == MINUS) {
        currTerm = parseTerm(currTerm)
      }
      currTerm
    }

    def parseParenExprTree(): ExprTree = {
      eat(LPAREN)
      val expr = parseExprTree
      eat(RPAREN)
      expr
    }

    def parseRecursiveExprTree(firstExpr: ExprTree): ExprTree = {
      currentToken.kind match {
        case AND => {
          readToken
          val rhs = parseExprTree
          And(firstExpr, rhs)
        }
        case OR => {
          readToken
          val rhs = parseExprTree
          Or(firstExpr, rhs)
        }
        case EQUALS => {
          readToken
          val rhs = parseExprTree
          Equals(firstExpr, rhs)
        }
        case LESSTHAN => {
          readToken
          val rhs = parseExprTree
          LessThan(firstExpr, rhs)
        }
        case PLUS | MINUS | TIMES | DIV => {
          parseTermList(firstExpr)
        }
        case LBRACKET => {
          readToken
          val index = parseExprTree
          ArrayRead(firstExpr, index)
        }
        case DOT => {
          readToken
          currentToken.kind match {
            case LENGTH => {
              ArrayLength(firstExpr)
            }
            case IDKIND => {
              val idStr = currentToken.asInstanceOf[ID].value
              val params = parseParams
              MethodCall(firstExpr, Identifier(idStr), params)
            }
            case _ => {
              fatal("Not a valid DOT operation")
            }
          }
        }
        case _ => {
          fatal("Invalid recursive statement.")
        }
      }
    }

    def parseExprTree(): ExprTree = {
      currentToken.kind match {
        // terminals
        case TRUE => {
          readToken
          True()
        }
        case FALSE => {
          readToken
          False()
        }
        case INTLITKIND => {
          val intValue = currentToken.asInstanceOf[INTLIT].value
          readToken
          new IntLit(intValue)
        }
        case STRLITKIND => {
          val strValue = currentToken.asInstanceOf[STRLIT].value
          readToken
          new StringLit(strValue)
        }
        case IDKIND => {
          val idStr = currentToken.asInstanceOf[ID].value
          val identifier = Identifier(idStr)
          readToken
          currentToken.kind match {
            case EQSIGN => {
              readToken
              val expr = parseExprTree
              Assign(identifier, expr)
            }
            case LBRACKET => {
              readToken
              val index = parseExprTree
              eat(RBRACKET)
              eat(EQSIGN)
              val expr = parseExprTree
              ArrayAssign(identifier, index, expr)
            }
            case _ => {
             fatal("Invalid Expr with IDKIND.")
            }
          }
        }
        case SELF => {
          new Self()
        }

        // non recursive start
        case NEW => {
          readToken
          currentToken.kind match {
            case INT => {
              readToken
              eat(LBRACKET)
              val size = parseExprTree
              eat(RBRACKET)
              NewIntArray(size)
            }
            case IDKIND => {
              val idStr = currentToken.asInstanceOf[ID].value
              eat(LPAREN)
              eat(RPAREN)
              New(Identifier(idStr))
            }
            case _ => {
              fatal("Not valid New statement.")
            }
          }
        }

        case BANG => {
          readToken
          Not(parseExprTree)
        }
        case LPAREN => {
          val expr = parseParenExprTree
          expr
        }
        case LBRACE => {
          readToken
          val exprList = parseBlock
          eat(RBRACE)
          Block(exprList)
        }
        case IF => {
          readToken
          val predicate = parseParenExprTree
          val ifExprList = parseExprTree
          readToken
          var elseExprList = None: Option[ExprTree]
          if (currentToken.kind == ELSE) {
            readToken
            elseExprList = Some(parseExprTree)
          }
          If(predicate, ifExprList, elseExprList)
        }
        case WHILE => {
          readToken
          val predicate = parseParenExprTree
          val body = parseParenExprTree
          While(predicate, body)
        }
        case PRINTLN => {
          val expr = parseParenExprTree
          Println(expr)
        }
        case STROF => {
          val expr = parseParenExprTree
          Strof(expr)
        }

        // recursive start
        case _ => {
          currentToken.kind match {
            case AND | OR | EQUALS | LESSTHAN | PLUS | MINUS | TIMES | DIV => {
              parseRecursiveExprTree(identifier)
            }
            case _ => {
              identifier
            }
          }
        }
      }
    }

    def parseParams: List[ExprTree] = {
      eat(LPAREN)
      var exprList = List[ExprTree]()
      currentToken.kind match {
        case RBRACE => {
          readToken
          exprList
        }
        case _ => {
          exprList :+= parseExprTree
          readToken
          while(currentToken.kind == COMMA) {
            readToken
            exprList :+= parseExprTree
            readToken
          }
          eat(RPAREN)
          exprList
        }
      }
    }

    // a block is a list of exprs
    def parseBlock(): List[ExprTree] = {
      var exprList = List[ExprTree]()

      readToken
      currentToken.kind match {
        case RBRACE => {
          eat(RBRACE)
          exprList
        }
        case _ => {
          exprList :+= parseExprTree
          while(currentToken.kind == SEMICOLON) {
            readToken
            exprList :+= parseExprTree
          }
          eat(RBRACE)
          exprList
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
      val exprFirst = parseExprTree
      val exprList = exprFirst::parseBlock
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
