package com

import scalariform.lexer.Token
import scalariform.parser.AstNode

/**
 * Created by amits on 1/10/15.
 */
class VisitorHelper {
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
}
