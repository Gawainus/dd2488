package slacc
package analyzer

import slacc.ast.Trees._
import slacc.utils._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {

    // Step 1: Collect symbols in declarations


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }
}
