package com

import org.scalastyle.scalariform.AbstractClassChecker

import scalariform.formatter.{FormatResult, FormatterState, ScalaFormatter}
import scalariform.lexer.ScalaLexer
import scalariform.parser.{CompilationUnit, AstNode, ScalaParser}

/**
 * Created by amits on 28/9/15.
 */
object Test {

def main (args: Array[String]) {
 val s =  """
  import java.net._
  import java.io.File
class C(@annotation(foo = {1 + 2}) n: Int){
def main() {
println("abc")
}
}
  """
  val tokens = ScalaLexer.tokenise(s)
  val scalaParser = new ScalaParser(tokens.toArray)
  val cu = scalaParser.safeParse(scalaParser.compilationUnitOrScript)
  val astList = cu.get.immediateChildren
  println(cu.get.immediateChildren(0).immediateChildren.toList.map(a => getSource(a)))
  //println(cu.get.immediateChildren(0).tokens.map())
  getSource(cu.get)
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

