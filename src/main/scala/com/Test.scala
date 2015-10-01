package com


import org.scalastyle.{PositionError, ScalariformChecker, ScalastyleError}

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


//    val s =
//      """
//        |package foobar
//        |import java.util._
//        |import sun.com.foobar
//        |import sun._
//        |
//        |class foobar {
//        |  val MyField1 = new Foobar("one")
//        |  var MyField2 = 2
//        |  val MyField3
//        |  var MyField4
//        |  val myField51; val MyField52 = 52
//        |  var myField61; var MyField62 = 62
//        |}
//      """.stripMargin
    val tokens = ScalaLexer.tokenise(s)
    val scalaParser = new ScalaParser(tokens.toArray)
    val cu = scalaParser.safeParse(scalaParser.compilationUnitOrScript)
//    val astList = cu.get.immediateChildren(0)

    val fieldNamesChecker = new FieldNamesChecker

    val illegalImportsChecker = new IllegalImportsChecker
    illegalImportsChecker.verify(cu.get)
//    fieldNamesChecker.verify(cu.get)
    //    println(localvisit(astList))
    //  println(cu.get.immediateChildren(0).immediateChildren.toList.map(a => getSource(a)))
    //  getSource(cu.get)
  }

  //  case class TmplClazz(t: TmplDef, subs: List[TmplClazz]) extends TreeVisit[TmplClazz]
  //  private def localvisit1(ast: Any): ListType = ast match {
  //    case t: TmplDef     => List(TmplClazz(Some(t.name.getText), Some(t.name.startIndex),
  //      localvisit(t.templateBodyOption)))
  //    case t: FunDefOrDcl => List(FunDefOrDclClazz(method(t), Some(t.nameToken.startIndex), localvisit(t.localDef)))
  //    case t: Any         => visit(t, localvisit)
  //  }
  private def localvisit(ast: Any): List[String] = ast match {
    case t: ImportClause => List(t.importExpr.toString) ++ localvisit(t.importExpr)
    case t: TmplDef => List(t.name.getText) ++ localvisit(t.templateBodyOption)
    case t: FunDefOrDcl => List(t.nameToken.getText) ++ localvisit(t.localDef)
    case t: Any => visit(t, localvisit)
  }

  abstract class AbstractImportChecker extends ScalariformChecker {
    case class ImportClauseVisit(t: ImportClause, importExpr: List[ImportClauseVisit], otherImportExprs: List[ImportClauseVisit]);

    def verify(ast: CompilationUnit): List[ScalastyleError] = {
      init()

      val it = for {
        t <- localvisit(ast.immediateChildren);
        f <- traverse(t)
      } yield {
          println("imports " + imports(t))
          PositionError(t.t.firstToken.offset)
        }

      it.toList
    }

    protected def init(): Unit = {}

    private[this] def traverse(t: ImportClauseVisit): List[ImportClauseVisit] = {
      val l = t.importExpr.map(traverse(_)).flatten ::: t.otherImportExprs.map(traverse(_)).flatten
      if (matches(t)) t :: l else l
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

    def matches(t: ImportClauseVisit): Boolean

    private[this] def localvisit(ast: Any): List[ImportClauseVisit] = ast match {
      case t: ImportClause => List(ImportClauseVisit(t, localvisit(t.importExpr), localvisit(t.otherImportExprs)))
      case t: Any => visit(t, localvisit)
    }
  }

  class IllegalImportsChecker extends AbstractImportChecker {
    val errorKey = "illegal.imports"

    val DefaultIllegalImports = "sun._"
    var illegalImportsList: List[String] = _
    var exemptImportsList: List[String] = _

    // sun._ => sun\.
    // sun.com.foobar => sun\.com\.foobar
    private def toMatchList(s: String) = {
      s.trim().split(" *, *").map(s => s.replaceAll("_$", "")).toList
    }

    override protected def init() = {
      illegalImportsList = toMatchList(getString("illegalImports", DefaultIllegalImports))
      exemptImportsList = toMatchList(getString("exemptImports", ""))
    }

    def matches(t: ImportClauseVisit): Boolean = {
      val list = imports(t)
      println(list.mkString)
      val revisedList = list diff exemptImportsList
      illegalImportsList.exists(ill => revisedList.exists(_.startsWith(ill)))
    }
  }

  class FieldNamesChecker extends ScalariformChecker {
    val DefaultRegex = "^[a-z][A-Za-z0-9]*$"
    val errorKey = "field.name"

    import scalariform.lexer.Tokens._

    def verify(ast: CompilationUnit): List[ScalastyleError] = {
      val regexString = getString("regex", DefaultRegex)
      val regex = regexString.r

      val it = for {List(left, right) <- ast.tokens.sliding(2)
                    if (left.tokenType == VAL || left.tokenType == VAR)}
        yield {
          println(right.getText)
        PositionError(right.offset, List(regexString))
      }

      println(it.toList)
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


