package com.skillw
package lisp.process

import lisp.scope.Context
import lisp.term.Expr

import lisp.util.*

object Eval {

  import lisp.term.Expr.*

  private def subs(expr: Expr, ctx: Context): Result[Expr] = expr match
    case v@Var(name,_) => ctx.get(v)
    case Com(exprs,index) => exprs.mapResult(subs(_, ctx)).map(Com(_,index))
    case App(app, args,index) => args.mapResult(subs(_, ctx)).map(App(app, _,index))
    case e => e.ok


  def eval(expr: Expr, ctx: Context): Result[Expr] = expr match
      case v@Var(_,_) => ctx.get(v)
      case Com(Nil,index) => Com(Nil,index).ok
      case Com(exprs,index) =>
        exprs.head.eval(ctx).mapResult{
          case Abs(a, _) => App(a, exprs.tail.toList, index).eval(ctx)
          case e => exprs.tail.mapResult(subs(_, ctx)).map(es => Com(e +: es, index))
        }
      case App(app, args,index) => args.mapResult(subs(_, ctx)).mapResult(app.apply(index,ctx,_)) 
      case Def(name, _value,index) =>
        _value.eval(ctx).map(value => 
          ctx.add(name, value)
          value
        )
      case expr => expr.ok
}
