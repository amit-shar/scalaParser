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

  private def localvisit(ast: Any): List[String] = ast match {
    case t: ImportClause => List(t.importExpr.toString) ++ localvisit(t.importExpr)
    case t: TmplDef => List(t.name.getText) ++ localvisit(t.templateBodyOption)
    case t: FunDefOrDcl => List(t.nameToken.getText) ++ localvisit(t.localDef)
    case t: Any => visit(t, localvisit)
  }

  abstract class AbstractImportChecker {
    var parameters = Map[String, String]();

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
        visit(t, localvisit)
      case t: Any => visit(t, localvisit)
    }
  }

  class IllegalImportsChecker extends AbstractImportChecker {
  }

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

  def visit[T](ast: Any, visitfn: (Any) => List[T]): List[T] = ast match {
    case a: AstNode => visitfn(a.immediateChildren)
    case t: Token => List()
    case Some(x) => visitfn(x)
    case xs@(_ :: _) => xs flatMap {
      visitfn(_)
    }
    case Left(x) => visitfn(x)
    case Right(x) => visitfn(x)
    case (l, r) => visitfn(l) ::: visitfn(r)
    case (x, y, z) => visitfn(x) ::: visitfn(y) ::: visitfn(z)
    case true | false | Nil | None => List()
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


