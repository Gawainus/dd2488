package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import slacc.analyzer.Types._

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
        while (currentToken.kind == BAD || currentToken.kind == CMT) {
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

    def parseFormal: Formal = {
      currentToken.kind match {
        case IDKIND => // a formal has to start with an ID
          val id = currentToken.asInstanceOf[ID]
          val formalName = id.value

          readToken
          eat(COLON)

          currentToken.kind match {
            // match type of the formal
            case INT =>
              readToken

              currentToken.kind match {
                case LBRACKET =>
                  readToken
                  eat(RBRACKET)
                  val formalId = Identifier(formalName).setPos(id)
                  Formal(IntArrayType(), formalId).setPos(id)

                case _ =>
                  val formalId = Identifier(formalName).setPos(id)
                  Formal(IntType(), formalId).setPos(id)
              }

            case BOOLEAN =>
              readToken
              val formalId = Identifier(formalName).setPos(id)
              Formal(BooleanType(), formalId).setPos(id)

            case STRING =>
              readToken
              val formalId = Identifier(formalName)
              Formal(StringType(), formalId).setPos(id)

            case UNIT =>
              readToken
              val formalId = Identifier(formalName)
              Formal(UnitType(), formalId).setPos(id)

            case _ =>
              if (currentToken.kind == IDKIND) {
                val typeName = currentToken.asInstanceOf[ID].value
                val formalId = Identifier(formalName)
                val userDefinedFormal = Formal(UserDefinedType(typeName), formalId).setPos(id)
                readToken
                userDefinedFormal
              }
              else {
                expected(IDKIND)
              }
          }
        case _ =>
          expected(IDKIND)
      }
    }

    def parseMethodReturnType: TypeTree = {
      eat(RPAREN)
      eat(COLON)

      val retTypeToken = currentToken
      retTypeToken.kind match {
        case INT =>
          readToken
          currentToken.kind match {
            case LBRACKET =>
              readToken
              eat(RBRACKET)
              eat(EQSIGN)
              val retType = IntArrayType()
              retType.setPos(retTypeToken)
              retType.setType(TIntArray)
              retType

            case _ =>
              eat(EQSIGN)
              val retType = IntType()
              retType.setPos(retTypeToken)
              retType.setType(TInt)
              retType
          }

        case BOOLEAN =>
          readToken
          eat(EQSIGN)
          val retType = BooleanType()
          retType.setPos(retTypeToken)
          retType.setType(TBoolean)
          retType

        case STRING =>
          readToken
          eat(EQSIGN)
          val retType = StringType()
          retType.setPos(retTypeToken)
          retType.setType(TString)
          retType

        case UNIT =>
          readToken
          eat(EQSIGN)

          val retType = UnitType()

          retType.setPos(retTypeToken)
          retType.setType(TUnit)
          retType

        case _ =>
          if (currentToken.kind == IDKIND) {

            val typeName = currentToken.asInstanceOf[ID].value
            val userDefinedType = UserDefinedType(typeName)
            userDefinedType.setPos(retTypeToken)

            readToken
            eat(EQSIGN)
            userDefinedType
          }
          else {
            expected(IDKIND)
          }
      }
    }

    def parseVarDecl: VarDecl = {
      // var Identifier: Type;
      val varToken = currentToken
      eat(VAR)

      val varIdToken = currentToken
      varIdToken.kind match {
        case IDKIND =>
          val varName = varIdToken.asInstanceOf[ID].value
          readToken
          eat(COLON)

          val varTypeToken = currentToken
          varTypeToken.kind match {
            case INT =>
              readToken

              currentToken.kind match {
                case LBRACKET =>
                  readToken
                  eat(RBRACKET)
                  eat(SEMICOLON)

                  val intArrayId = Identifier(varName)
                  intArrayId.setPos(varIdToken)
                  VarDecl(IntArrayType().setPos(varTypeToken), intArrayId).setPos(varToken)

                case _ =>
                  eat(SEMICOLON)

                  val intId = Identifier(varName)
                  intId.setPos(varIdToken)
                  VarDecl(IntType().setPos(varTypeToken), intId).setPos(varToken)
              }

            case BOOLEAN =>
              readToken
              eat(SEMICOLON)

              val boolId = Identifier(varName)
              boolId.setPos(varIdToken)
              VarDecl(BooleanType().setPos(varTypeToken), boolId).setPos(varToken)

            case STRING =>
              readToken
              eat(SEMICOLON)

              val strId = Identifier(varName)
              strId.setPos(varIdToken)
              VarDecl(StringType().setPos(varTypeToken), strId).setPos(varToken)

            case UNIT =>
              readToken
              eat(SEMICOLON)

              val unitId = Identifier(varName)
              unitId.setPos(varIdToken)
              VarDecl(UnitType().setPos(varTypeToken), unitId).setPos(varToken)

            case _ =>
              if (currentToken.kind == IDKIND) {

                val typeName = currentToken.asInstanceOf[ID].value
                val userDefinedVarId = Identifier(varName)
                userDefinedVarId.setPos(varIdToken)

                readToken
                eat(SEMICOLON)
                VarDecl(UserDefinedType(typeName).setPos(varTypeToken), userDefinedVarId).setPos(varToken)
              }
              else {
                fatal("Unknown Kind")
              }
          }
        case _ =>
          expected(IDKIND)
      }
    }

    // math part
    def parseFactor: ExprTree = {
      val factorToken = currentToken
      factorToken.kind match {

        case INTLITKIND =>
          val intValue = factorToken.asInstanceOf[INTLIT].value
          readToken
          IntLit(intValue).setPos(factorToken)

        case LPAREN =>
          parseParenExprTree

        case _ =>
          parseExprTree
      }
    }

    def parseFactorList(firstExpr: ExprTree): ExprTree = {
      var currFactor = firstExpr

      while (currentToken.kind == TIMES | currentToken.kind == DIV) {
        currentToken.kind match {
          case TIMES =>
            readToken
            currFactor = Times(currFactor, parseFactor)

          case _ =>
            readToken
            currFactor = Div(currFactor, parseFactor)

        }
      }
      currFactor
    }

    def parseTerm(lhs: ExprTree): ExprTree = {
      currentToken.kind match {
        case PLUS =>
          readToken
          val firstExpr = parseFactor
          val rhs = parseFactorList(firstExpr)
          Plus(lhs, rhs)

        case MINUS =>
          readToken
          val firstExpr = parseFactor
          val rhs = parseFactorList(firstExpr)
          Minus(lhs, rhs)

        case _ =>
          fatal("Not a valid term.")
      }
    }

    def parseTermList(firstExpr: ExprTree): ExprTree = {
      var currTerm = parseFactorList(firstExpr)
      while (currentToken.kind == PLUS | currentToken.kind == MINUS) {
        currTerm = parseTerm(currTerm)
      }
      currTerm
    }

    def parseParenExprTree: ExprTree = {
      eat(LPAREN)
      val expr = parseExprTree
      eat(RPAREN)
      expr
    }

    def parseRecursiveExprTree(firstExpr: ExprTree): ExprTree = {
      currentToken.kind match {
        case AND =>
          readToken
          val rhs = parseExprTree
          parseRecursiveExprTree(And(firstExpr, rhs))

        case OR =>
          readToken
          val rhs = parseExprTree
          parseRecursiveExprTree(Or(firstExpr, rhs))

        case EQUALS =>
          readToken
          val rhs = parseExprTree
          Equals(firstExpr, rhs)

        case LESSTHAN =>
          readToken
          val rhs = parseExprTree
          LessThan(firstExpr, rhs)

        case PLUS | MINUS | TIMES | DIV =>
          parseTermList(firstExpr)

        case LBRACKET =>
          readToken
          val index = parseExprTree
          eat(RBRACKET)
          parseRecursiveExprTree(ArrayRead(firstExpr, index))

        case DOT =>
          readToken
          currentToken.kind match {
            case LENGTH =>
              readToken
              parseRecursiveExprTree(ArrayLength(firstExpr))

            case IDKIND =>
              val idStr = currentToken.asInstanceOf[ID].value
              readToken
              val params = parseParams
              parseRecursiveExprTree(MethodCall(firstExpr, Identifier(idStr), params))

            case _ =>
              fatal("Not a valid DOT operation")
        }
        case _ =>
          firstExpr
      }
    }

    def parseExprTree: ExprTree = {
      currentToken.kind match {
        // terminals
        case TRUE =>
          readToken
          parseRecursiveExprTree(True().setPos(currentToken))

        case FALSE =>
          readToken
          parseRecursiveExprTree(False().setPos(currentToken))

        case INTLITKIND =>
          val intValue = currentToken.asInstanceOf[INTLIT].value
          readToken
          parseRecursiveExprTree(IntLit(intValue).setPos(currentToken))

        case STRLITKIND =>
          val strValue = currentToken.asInstanceOf[STRLIT].value
          readToken
          parseRecursiveExprTree(StringLit(strValue).setPos(currentToken))

        case IDKIND =>
          val idStr = currentToken.asInstanceOf[ID].value
          val identifier = Identifier(idStr).setPos(currentToken)

          readToken
          currentToken.kind match {
            case EQSIGN =>
              readToken

              val firstExpr = parseExprTree
              val expr = parseFactorList(firstExpr)
              Assign(identifier, expr)

            case LBRACKET =>
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

            case _ =>
              parseRecursiveExprTree(identifier)

          }
        case SELF =>
          val selfExpr = Self().setPos(currentToken)
          readToken
          parseRecursiveExprTree(selfExpr)

        // non recursive start
        case NEW =>
          val newToken = currentToken
          readToken

          currentToken.kind match {
            case INT =>
              readToken
              eat(LBRACKET)
              val size = parseExprTree
              eat(RBRACKET)
              val newIntArr = NewIntArray(size).setPos(newToken)
              parseRecursiveExprTree(newIntArr)

            case IDKIND =>
              val idStr = currentToken.asInstanceOf[ID].value
              readToken
              eat(LPAREN)
              eat(RPAREN)
              val newObj = New(Identifier(idStr)).setPos(newToken)
              parseRecursiveExprTree(newObj)

            case _ =>
              fatal("Not valid New statement.")
          }

        case BANG =>
          readToken
          Not(parseExprTree).setPos(currentToken)

        case LPAREN =>
          val expr = parseParenExprTree
          expr

        case LBRACE =>
          val blockToken = currentToken
          readToken

          val exprList = parseBlock
          Block(exprList).setPos(blockToken)

        case IF =>
          val ifToken = currentToken
          readToken

          val predicate = parseParenExprTree
          val ifExprList = parseExprTree
          var elseExprList = None: Option[ExprTree]
          if (currentToken.kind == ELSE) {
            readToken
            elseExprList = Some(parseExprTree)
          }
          If(predicate, ifExprList, elseExprList).setPos(ifToken)

        case WHILE =>
          val whileToken = currentToken
          readToken

          val predicate = parseParenExprTree
          val body = parseExprTree
          While(predicate, body).setPos(whileToken)

        case PRINTLN =>
          val printlnToken = currentToken
          readToken

          val expr = parseParenExprTree
          Println(expr).setPos(printlnToken)

        case STROF =>
          val strOfToken = currentToken
          readToken

          val expr = parseParenExprTree
          StrOf(expr).setPos(strOfToken)

        case _ =>
          fatal("Invalid Expr: " + currentToken.toString + " at " + currentToken.position)
      }
    }

    def parseParams: List[ExprTree] = {
      eat(LPAREN)
      var exprList = List[ExprTree]()

      currentToken.kind match {
        case RPAREN =>
          readToken
          exprList

        case _ =>
          exprList :+= parseExprTree
          while(currentToken.kind == COMMA) {
            readToken
            exprList :+= parseExprTree
          }
          eat(RPAREN)
          exprList
      }
    }

    // a block is a list of exprs
    def parseBlock: List[ExprTree] = {
      var exprList = List[ExprTree]()

      currentToken.kind match {
        case RBRACE =>
          readToken
          exprList

        case _ =>
          exprList :+= parseExprTree
          while(currentToken.kind == SEMICOLON) {
            readToken
            exprList :+= parseExprTree
          }
          eat(RBRACE)
          exprList
      }
    }

    def parseMethodBody: (List[VarDecl], List[ExprTree]) = {
      eat(LBRACE)
      // parse the body
      var varList = List[VarDecl]()
      while (currentToken.kind == VAR) {
        varList :+= parseVarDecl
      }
      var exprList = parseExprTree :: Nil

      currentToken.kind match {
        case SEMICOLON =>
          readToken
          exprList = exprList ::: parseBlock

        case _ => eat(RBRACE)
      }
      (varList, exprList)
    }

    def parseMethodDecl(id: ID, locToken: Token): MethodDecl = {
      val methodId = Identifier(id.value)
      readToken
      eat(LPAREN)

      currentToken.kind match {
        case IDKIND =>
          var formalList = List[Formal]()
          formalList :+= parseFormal
          while (currentToken.kind == COMMA) {
            readToken
            formalList :+= parseFormal
          }
          val retType = parseMethodReturnType
          val (varList, exprList) = parseMethodBody
          MethodDecl(retType, methodId, formalList, varList, exprList , exprList.last).setPos(locToken)

        case RPAREN =>
          val retType = parseMethodReturnType
          val (varList, exprList) = parseMethodBody
          MethodDecl(retType, methodId, List[Formal](), varList, exprList, exprList.last).setPos(locToken)

        case _ =>
          ctx.reporter.fatal("Expecting arguments or right parenthesis.")
      }
    }

    def parseClass(id: ID, locToken: Token): ClassDecl = {
      val classId = Identifier(id.value)
      readToken
      var parent: Option[Identifier] = None
      if (currentToken.kind == LESSTHAN) {
        readToken
        eat(COLON)
        currentToken.kind match {
          case IDKIND =>
            parent = Some(Identifier(currentToken.asInstanceOf[ID].value))
            readToken

          case _ =>
            eat(IDKIND)
        }
      }
      eat(LBRACE)

      var varList = List[VarDecl]()
      while (currentToken.kind == VAR) {
        varList :+= parseVarDecl
      }

      var methodList = List[MethodDecl]()
      while (currentToken.kind == METHOD) {
        val methodToken = currentToken
        readToken

        currentToken match {
          case idToken: ID =>
            methodList :+= parseMethodDecl(idToken, methodToken)

          case _ => throw new ClassCastException
        }
      }
      eat(RBRACE)

      ClassDecl(classId, parent, varList, methodList).setPos(locToken)
    }

    def parseGoal: Program = {
      var classList = List[ClassDecl]()
      while (currentToken.kind == CLASS) {
        val classToken = currentToken
        readToken

        currentToken match {
          case idToken: ID =>
            classList :+= parseClass(idToken, classToken)

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
          val methodToken = currentToken
          readToken

          currentToken match {
            case idToken: ID =>
              val mainMethod = MainMethod(parseMethodDecl(idToken, methodToken))
              Program(mainMethod, classList)

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
