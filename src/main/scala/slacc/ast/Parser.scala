package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  val firstOfFactor = List[TokenKind](TIMES, DIV)
  val firstOfTerm = List[TokenKind](PLUS, MINUS)
  val symbolsOfRecursion = List[TokenKind](AND, OR, EQUALS, LESSTHAN, PLUS, MINUS, TIMES, DIV,
    LBRACKET, DOT)

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)
    var mainMethodFound: Boolean = false

    def readToken: Unit = {
      if (tokens.hasNext) {

        // uses nextToken from the Lexer trait
        currentToken = tokens.next
        // println(currentToken.toString)
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
              val userDefinedFormal = new Formal(UserDefinedType(
                Identifier(currentToken.asInstanceOf[ID].value)),
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

    def parseMethodReturnType: TypeTree = {
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
              val userDefinedVar = new VarDecl(UserDefinedType(
                Identifier(currentToken.asInstanceOf[ID].value)),
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
          val firstExpr = parseExprTree
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
          val firstExpr = parseFactor
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
          parseRecursiveExprTree(And(firstExpr, rhs))
        }
        case OR => {
          readToken
          val rhs = parseExprTree
          parseRecursiveExprTree(Or(firstExpr, rhs))
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
          eat(RBRACKET)
          parseRecursiveExprTree(ArrayRead(firstExpr, index))
        }
        case DOT => {
          readToken
          currentToken.kind match {
            case LENGTH => {
              readToken
              parseRecursiveExprTree(ArrayLength(firstExpr))
            }
            case IDKIND => {
              val idStr = currentToken.asInstanceOf[ID].value
              readToken
              val params = parseParams
              parseRecursiveExprTree(MethodCall(firstExpr, Identifier(idStr), params))
            }
            case _ => {
              fatal("Not a valid DOT operation")
            }
          }
        }
        case _ => {
          firstExpr
        }
      }
    }

    def parseExprTree: ExprTree = {
      currentToken.kind match {
        // terminals
        case TRUE => {
          readToken
          parseRecursiveExprTree(True())
        }
        case FALSE => {
          readToken
          parseRecursiveExprTree(False())
        }
        case INTLITKIND => {
          val intValue = currentToken.asInstanceOf[INTLIT].value
          readToken
          parseRecursiveExprTree(IntLit(intValue))
        }
        case STRLITKIND => {
          val strValue = currentToken.asInstanceOf[STRLIT].value
          readToken
          parseRecursiveExprTree(StringLit(strValue))
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
              if (currentToken.kind == EQSIGN) {
                readToken
                val expr = parseExprTree
                ArrayAssign(identifier, index, expr)
              }
              else {
                ArrayRead(identifier, index)
              }
            }
            case _ => {
              parseRecursiveExprTree(identifier)
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
          val body = parseExprTree
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

        case _ => {
          fatal("Invalid Expr: " + currentToken.toString + " at " + currentToken.position)
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
    def parseBlock: List[ExprTree] = {
      var exprList = List[ExprTree]()

      currentToken.kind match {
        case RBRACE => {
          readToken
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

    def parseMethodBody: (List[VarDecl], List[ExprTree]) = {
      eat(LBRACE)
      // parse the body
      var varList = List[VarDecl]();
      while (currentToken.kind == VAR) {
        varList :+= parseVarDecl
      }
      var exprList = parseExprTree :: Nil

      currentToken.kind match {
        case SEMICOLON => {
          readToken
          exprList = exprList ::: parseBlock
        }
        case _ => eat(RBRACKET)
      }
      (varList, exprList)
    }

    def parseMethodDecl(id: ID): MethodDecl = {
      val methodId = new Identifier(id.value)
      eat(IDKIND)
      eat(LPAREN)

      currentToken.kind match {
        case IDKIND => {
          var formalList = List[Formal]()
          formalList :+= parseFormal
          while (currentToken.kind == COMMA) {
            readToken
            formalList :+= parseFormal
          }
          val retType = parseMethodReturnType
          val (varList, exprList) = parseMethodBody
          new MethodDecl(retType, methodId, formalList, varList, exprList , exprList.last)
        }

        case RPAREN => {
          val retType = parseMethodReturnType
          val (varList, exprList) = parseMethodBody
          new MethodDecl(retType, methodId, List[Formal](), varList, exprList, exprList.last)
        }
        case _ => {
          ctx.reporter.fatal("Expecting arguments or right parenthesis.")
        }
      }
    }

    def parseClass(id: ID): ClassDecl = {
      val classId = new Identifier(id.value)
      readToken
      var parent: Option[Identifier] = None
      if (currentToken.kind == LESSTHAN) {
        readToken
        eat(COLON)
        currentToken.kind match {
          case IDKIND => {
            parent = Some(Identifier(currentToken.asInstanceOf[ID].value))
            readToken
          }
          case _ => {
            eat(IDKIND)
          }
        }
      }
      eat(LBRACE)

      var varList = List[VarDecl]()
      while (currentToken.kind == VAR) {
        varList :+= parseVarDecl
        readToken
      }

      var methodList = List[MethodDecl]()
      while (currentToken.kind == METHOD) {
        eat(METHOD)
        currentToken match {
          case idToken: ID => {
            methodList :+= parseMethodDecl(idToken)
            readToken
          }
          case _ => throw new ClassCastException
        }
      }
      eat(RBRACE)

      new ClassDecl(classId, parent, varList, methodList)
    }

    def parseGoal(): Program = {
      var classList = List[ClassDecl]()
      while (currentToken.kind == CLASS) {
        readToken
        currentToken match {
          case idToken: ID => {
            classList :+= parseClass(idToken)
          }
          case _ => throw new ClassCastException
        }
      }
      if (currentToken.kind == METHOD) {
        if (mainMethodFound) {
          ctx.reporter.fatal(
            "Exactly one MainMethod declared after class declarations. At least 2 found.")
        }
        else {
          mainMethodFound = true
          eat(METHOD)
          currentToken match {
            case idToken: ID => {
              val mainMethod = new MainMethod(parseMethodDecl(idToken))
              Program(mainMethod, classList)
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
