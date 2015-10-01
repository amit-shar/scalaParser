package com

import scalariform.lexer.ScalaLexer
import scalariform.parser._
import scalariform.lexer.Token

/**
 * Created by amits on 28/9/15.
 */
object Test {
  def main(args: Array[String]) {
    val s =
      scala.io.Source.fromFile("src/main/resources/Example.scala").mkString
    val tokens = ScalaLexer.tokenise(s)
    val scalaParser = new ScalaParser(tokens.toArray)
    val cu = scalaParser.safeParse(scalaParser.compilationUnitOrScript)
    //    val astList = cu.get.immediateChildren(0)
    val fieldNamesChecker = new FieldNamesChecker

    val illegalImportsChecker = new IllegalImportsChecker
    val imports = illegalImportsChecker.verify(cu.get)

    println(imports)
    println(fieldNamesChecker.verify(cu.get))
  }

  /*private def localvisit(ast: Any): List[String] = ast match {
    case t: ImportClause => List(t.importExpr.toString) ++ localvisit(t.importExpr)
    case t: TmplDef => List(t.name.getText) ++ localvisit(t.templateBodyOption)
    case t: FunDefOrDcl => List(t.nameToken.getText) ++ localvisit(t.localDef)
    case t: Any => visitor.visit(t, localvisit)
  }

*/
  class FieldNamesChecker {

    import scalariform.lexer.Tokens._

    def verify(ast: CompilationUnit): List[String] = {
      val it = for {List(left, right) <- ast.tokens.sliding(2)
                    if (left.tokenType == VAL || left.tokenType == VAR)}
        yield {
          right.getText
        }
      it.toList
    }
  }

  def getSource(astNode: AstNode): String = {
    val sb = new StringBuilder
    for (token ← astNode.tokens) {
      if (token != astNode.tokens.head)
      // sb.append(hiddenPredecessors(token).rawText)
        sb.append(token.rawText)
    }
    //println(sb.toString())
    sb.toString
  }
}


