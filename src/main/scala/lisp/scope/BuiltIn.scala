package com.skillw
package lisp.scope

import lisp.term.Expr
import lisp.term.Expr.*
import lisp.term.Applicable.*

import lisp.util.*

import scala.collection.mutable.ListBuffer


def builtIn(todo: Int => Context => List[Expr] => Result[Expr]) = Abs(BuiltIn(todo))

def evalToNumbers(ctx: Context, args: List[Expr]) = {
  args.mapResult(_.eval(ctx)).map(_.map{
    case Num(value,_) => Ok(value)
    case e => Err.of(s"Invalid argument $e, it should be a number!",e.pos)
  }.sequence
  ).flatten
}



object DefaultContext extends BuiltInContext {

  addNums("+")(index => ctx => args => Num(args.sum).ok)
  addNums("-")(index => ctx => {
    case only :: Nil => Num(-only).ok
    case first :: rest => Num(rest.foldLeft(first)(_ - _)).ok
    case _ => Err.of("Empty - arguments!", index)
  })
  addNums("*")(index => ctx => args => Num(args.product).ok)
  addNums("/")(index => ctx => {
    case only :: Nil => Num(1 / only).ok
    case first :: rest => Num(rest.foldLeft(first)(_ / _)).ok
    case _ => Err.of("Empty / arguments!", index)
  })
  add("=")(index => ctx => args => {
    args.mapResult(_.eval(ctx)).mapResult {
      case only :: Nil => Num(1).ok
      case first :: rest => Num(if (rest.forall(_ == first)) 1 else 0).ok
      case _ => Err.of("Empty = arguments!", index)
    }
  })
  addNums("<")(index => ctx => {
    case first :: rest => Num(if (rest.forall(first < _)) 1 else 0).ok
    case _ => Err.of("Invalid < arguments!", index)
  })
  addNums(">")(index => ctx => {
    case first :: rest => Num(if (rest.forall(first > _)) 1 else 0).ok
    case _ => Err.of("Invalid > arguments!", index)
  })
  add("if")(index => ctx => {
    case cond :: thenExpr :: elseExpr :: Nil => cond.eval(ctx).mapResult { cond =>
      if (cond == Num(1)) thenExpr.eval(ctx) else elseExpr.eval(ctx)
    }
    case _ => Err.of("Invalid if arguments!", index)
  })

  add("_"){ index => ctx => args => Ok(Num(1)) }
  add("cond")(index => ctx => args => {
    var result: Result[Expr] = Ok(Com())
    util.boundary {
      for (elem <- args) {
        elem match
          case Com(cond :: expr :: Nil, _) => cond.eval(ctx).mapResult {
            case Num(1, _) =>
              result = expr.eval(ctx)
              util.boundary.break()
            case _ => result
          } match
            case e: Err => result = e; util.boundary.break()
            case _ => ()
          case arg =>
            result = Err.of(s"Invalid cond argument $arg!", arg.pos)
            util.boundary.break()
      }
    }
    result
  })
  add("print")(index => ctx => {
    _.mapResult(_.eval(ctx)) match {
      case Ok(value) => value.foreach(println); Ok(value.lastOption.orElse(Some(Num(0))).get)
      case e: Err => e
    }
  })
}
class BuiltInContext{
  val builtIns = ListBuffer[(String, Expr)]()
  
  def add(name: String)(todo: Int => Context => List[Expr] => Result[Expr]) = {
    builtIns += name -> builtIn(todo)
  }
  def addNums(name: String)(todo: Int => Context => Seq[Double] => Result[Expr]) = {
    builtIns += name -> builtIn(index => ctx => {
      evalToNumbers(ctx, _) match
        case Ok(nums) => todo(index)(ctx)(nums)
        case e: Err => e
    })
  }

  def copy = {
    val context = new BuiltInContext
    context.builtIns ++= builtIns
    context
  }
}


