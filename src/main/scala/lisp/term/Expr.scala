package com.skillw
package lisp.term

import lisp.process.Eval
import lisp.scope.Context

import lisp.util.Result

enum Expr {
  case Var(name: String,index:Int = -1)
  case Num(value: Double,index:Int = -1)
  case Str(value: String,index:Int = -1)
  case Com(exprs: Seq[Expr] = List(),index:Int = -1)
  case Abs(abs: Applicable,index:Int = -1)
  case Def(name: String, expr: Expr,index:Int = -1)
  case App(body: Applicable, args: List[Expr],index:Int = -1)

  def pos: Int = this match
    case Var(_,index) => index
    case Num(_,index) => index
    case Str(_,index) => index
    case Com(_,index) => index
    case Abs(_,index) => index
    case Def(_,_,index) => index
    case App(_,_,index) => index
  
  def eval(ctx: Context): Result[Expr] = Eval.eval(this, ctx)
  

  def toJava: Any = this match
    case Num(value,_) => value
    case Str(value,_) => value
    case _ => this

  def raw: String = {
    this match
      case Str(value,_) => s"\"$value\""
      case Com(single :: Nil,_) => single.raw
      case Com(exprs,_) => s"(${exprs.map(_.raw).mkString(" ")})"
      case Def(name, expr,_) => s"(define $name ${expr.raw})"
      case App(app, args,_) => s"($app ${args.map(_.raw).mkString(" ")})"
      case _ => toString
  }

  override def toString: String = {
    this match {
      case Var(name,_) => name
      case Num(value,_) => value.toString
      case Str(value,_) => value
      case Abs(abs,_) => abs.toString
      case Com(single :: Nil,_) => single.toString
      case Com(exprs,_) => s"(${exprs.mkString(" ")})"
      case Def(name, expr,_) => s"(define $name $expr)"
      case App(app, args,_) => s"($app ${args.mkString(" ")})"
    }
  }

  override def equals(obj: Any): Boolean = (this, obj) match
    case (Var(name1,_), Var(name2,_)) => name1 == name2
    case (Num(value1,_), Num(value2,_)) => value1 == value2
    case (Str(value1,_), Str(value2,_)) => value1 == value2
    case (Com(exprs1,_), Com(exprs2,_)) => exprs1 == exprs2
    case (Abs(abs1,_), Abs(abs2,_)) => abs1 == abs2
    case (Def(name1, expr1,_), Def(name2, expr2,_)) => name1 == name2 && expr1 == expr2
    case (App(app1, args1,_), App(app2, args2,_)) => app1 == app2 && args1 == args2
    case (_, _) => false
}

trait Applicable{
  def apply(at:Int,ctx: Context,args:List[Expr]):Result[Expr]
}
object Applicable{
  case class Lambda(params: Seq[String], body: Expr) extends Applicable {
    override def apply(at:Int,ctx: Context, args: List[Expr]): Result[Expr] = {
      val newCtx = params.zip(args).foldLeft(ctx.copy())((ctx, pa) => ctx.add(pa._1, pa._2))
      body.eval(newCtx)
    }

    override def toString: String = {
      s"(lambda (${params.mkString(" ")}) $body)"
    }
  }

  case class BuiltIn(todo: Int => Context => List[Expr] => Result[Expr]) extends Applicable {
    override def apply(at:Int,ctx: Context, args: List[Expr]): Result[Expr] = todo(at)(ctx)(args)

    override def toString: String = "BuiltIn"
  }
}
