package slacc
package code

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._
import slacc.analyzer.Types.{TInt, TString}
import slacc.ast.Trees._
import slacc.utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {

    /** Writes the proper .class file in a given directory.
      * An empty string for dir is equivalent to "./".
      * */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      val classFile = new cafebabe.ClassFile(ct.getName, None)
      classFile.setSourceFile(ct.getName + ".java")
      classFile.addDefaultConstructor


      for (varDecl <- ct.vars) {

      }

      for (mt <- ct.methods) {
        var strList = List[String]()
        for (arg <- mt.args ) {
          strList :+= arg.getName
        }
        val ch = classFile.addMethod(mt.retType.toString, mt.getName, strList).codeHandler
        generateMethodCode(ch , mt)
      }
      classFile.writeToFile(dir + ct.getName + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      def geneCodeForExpr(ch: CodeHandler, expr: ExprTree): Unit = {

        expr match {

          case and: And =>

          case or: Or =>

          case lt: LessThan =>


          case eq: Equals =>

          case p: Plus =>
            p.getType match {
              case TInt =>
                geneCodeForExpr(ch, p.lhs)
                geneCodeForExpr(ch, p.rhs)
                ch << IADD

              case TString =>

              case _ =>
            }

          case mi: Minus =>
            geneCodeForExpr(ch, mi.lhs)
            geneCodeForExpr(ch, mi.rhs)
            ch << ISUB

          case mul: Times =>
            geneCodeForExpr(ch, mul.lhs)
            geneCodeForExpr(ch, mul.rhs)
            ch << IMUL


          case div: Div =>
            geneCodeForExpr(ch, div.lhs)
            geneCodeForExpr(ch, div.rhs)
            ch << IDIV

          case arrRead: ArrayRead =>

          case arrLen: ArrayLength =>

          case mc: MethodCall =>

          case intLit: IntLit =>
          case strLit: StringLit =>

          case t: True =>

          case f: False =>

          case id: Identifier =>

          case s: Self =>

          case nia: NewIntArray =>

          case n: New =>

          case not: Not =>
            geneCodeForExpr(ch, not.expr)

          case b: Block =>
            for (e <- b.exprs) {
              geneCodeForExpr(ch, e)
            }

          case i: If =>
            geneCodeForExpr(ch, i.cond)

            geneCodeForExpr(ch, i.thn)
            if (i.els.isDefined) {
              geneCodeForExpr(ch, i.els.get)
            }

          case w: While =>
            geneCodeForExpr(ch, w.cond)
            geneCodeForExpr(ch, w.body)
          case pr: Println =>
            ch <<
              GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
              Ldc(pr.expr.asInstanceOf[StringLit].value) <<
              InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
              RETURN

          case ass: Assign =>
            geneCodeForExpr(ch, ass.id)
            geneCodeForExpr(ch, ass.expr)

          case arrAss: ArrayAssign =>
            geneCodeForExpr(ch, arrAss.id)
            geneCodeForExpr(ch, arrAss.expr)
            geneCodeForExpr(ch, arrAss.index)

          case strOf: StrOf =>



          case _ =>
        }

      }

      val methSym = mt.getSymbol

      // TODO: Emit code
      for (expr <- mt.exprs) {
        geneCodeForExpr(ch, expr)
      }

      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.files.head.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main method
    // ...
    val classFile = new cafebabe.ClassFile("Main", None)
    classFile.setSourceFile("Main.java")
    classFile.addDefaultConstructor
    val mainMethod = prog.main.main
    val ch = classFile.addMainMethod.codeHandler
    generateMethodCode(ch, mainMethod)

    classFile.writeToFile(outDir + "Main.class")
  }
}
