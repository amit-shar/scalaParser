package com

import scalariform.lexer.Token
import scalariform.parser.{FunDefOrDcl, BlockImportExpr, CompilationUnit, ImportClause}

/**
 * Created by amits on 1/10/15.
 */
abstract class AbstractImportChecker {
  var parameters = Map[String, String]();
  val visitor = new VisitorHelper()

  case class ImportClauseVisit(t: ImportClause, importExpr: List[ImportClauseVisit], otherImportExprs: List[ImportClauseVisit]);

  def verify(ast: CompilationUnit): List[String] = {
    val it = for {t <- localvisit(ast.immediateChildren); f <- traverse(t)} yield {
      imports(t).mkString
    }
    it
  }

  private[this] def traverse(t: ImportClauseVisit): List[ImportClauseVisit] = {
    val l = t.importExpr.map(traverse(_)).flatten ::: t.otherImportExprs.map(traverse(_)).flatten
    t :: l
  }

  private[this] def imports(tokens: List[Token]): String = {
    tokens.foldLeft("")((a, b) => a + b.text)
  }

  private[this] def imports(t: BlockImportExpr): List[String] = {
    val is = t.importSelectors

    val firsts = is.firstImportSelector.firstToken.text ::
      is.otherImportSelectors.map(_._2).map(is => is.firstToken.text)
    firsts.map(f => imports(t.prefixExpr.tokens) + f)
  }

  protected final def imports(t: ImportClauseVisit): List[String] = {
    t.t.importExpr match {
      case t: BlockImportExpr => imports(t)
      case _ => List(imports(t.t.importExpr.tokens))
    }
  }

  private[this] def localvisit(ast: Any): List[ImportClauseVisit] = ast match {
    case t: ImportClause => List(ImportClauseVisit(t, localvisit(t.importExpr), localvisit(t.otherImportExprs)))
    case t: FunDefOrDcl =>
      println(t)
      visitor.visit(t, localvisit)
    case t: Any => visitor.visit(t, localvisit)
  }
}

class IllegalImportsChecker extends AbstractImportChecker {
}
