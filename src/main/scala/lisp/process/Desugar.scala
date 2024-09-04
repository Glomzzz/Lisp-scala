package com.skillw
package lisp.process

import lisp.term.Applicable.Lambda
import lisp.term.Elem
import lisp.term.Elem.*
import lisp.term.Expr
import lisp.util.*

import lisp.term.Expr.Abs

object Desugar {

   extension (list:Seq[Elem])
    def parseNames = list.mapResult({
      case Var(index, name) => name.ok
      case a => Err.of(s"Invalid lambda parameter: $a", a.pos)
    })

  def desugar(elem:Elem): Result[Expr] = {
    var err : Option[Err] = None
    elem match {
      case Var(index, name) => Expr.Var(name, index).ok
      case Num(index, value) => Expr.Num(value, index).ok
      case Str(index, value) => Expr.Str(value, index).ok
      case Com(index, exprs*) =>
        exprs match
          case v@Var(index, "lambda") :: Com(_, params*) :: body :: Nil =>
            (params.parseNames,body.desugar)
              .map((params, body) => Expr.Abs(Lambda(params, body), index))
          case Var(index, "define") :: elems :: value :: Nil =>
            elems match
              case Var(index, name) => value.desugar.map(Expr.Def(name, _, index))
              case Com(index,elems*) => elems match
                case Var(index, name) :: Nil => value.desugar.map(Expr.Def(name, _, index))
                case Var(index, name)::_params =>
                    (_params.parseNames, value.desugar)
                      .map((params, body) => Expr.Def(name, Abs(Lambda(params,body)), index))
                case e => Err.of(s"Invalid define: $e", index)
              case e => Err.of(s"Invalid define: $e", index)
          case _ => exprs.mapResult(desugar).map(Expr.Com(_, index))
    }
  }

}
