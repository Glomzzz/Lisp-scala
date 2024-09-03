package com.skillw

import Expr.*

import scala.collection.mutable.ListBuffer

sealed trait Expr{
  def eval(ctx:Context):Expr = evalExpr(this,ctx)
}
sealed trait Apply extends Expr{
  def apply(ctx:Context,args:List[Expr]): Expr
}
object Expr {
  case class Str(value: String) extends Expr
  case class Num(value: Double) extends Expr
  case class Var(name: String) extends Expr
  case class Combination(exprs:List[Expr]) extends Expr
  case class Def(name:String,body:Expr) extends Expr
  case class Lam(params:List[String], body:Expr) extends Apply{
    override def apply(ctx: Context, args: List[Expr]): Expr = body.eval(applyCtx(params,args,ctx))
  }
  case class BuiltIn(body:List[Expr] => Context => Expr) extends Apply{
    override def apply(ctx: Context, args: List[Expr]): Expr = body(args)(ctx)
  }
}


type Context = ListBuffer[(String, Expr)]

private def get(ctx: Context)(target: String): Option[Expr] = {
  ctx.indices.collectFirst {
    case i if ctx(i)._1 == target =>
      val (_, expr) = ctx(i)
      val result = expr.eval(ctx)
      if target == "g1" then
        println(s"g1 $expr")
        println(s"g1 $result")
      ctx(i) = (target, result)
      result
  }
}
private def getNonNull(ctx:Context)(target:String): Expr = get(ctx)(target).getOrElse(throw new Exception(s"undefined variable $target"))

private def substitute(ctx:Context)(expr:Expr):Expr = expr match
  case Var(target) => getNonNull(ctx)(target)
  case Combination(exprs) => Combination(exprs.map(substitute(ctx)))
  case expr => expr

private def applyCtx(params:Seq[String],args:Seq[Expr],ctx:Context):Context = {
  val newCtx = ListBuffer[(String,Expr)]()
  for (i <- params.indices) {
    newCtx.addOne((params(i),substitute(ctx)(args(i))))
  }
  newCtx ++ ctx
}

def evalExpr(expr:Expr, ctx:Context):Expr = {
  expr match
    case Def(name,body) =>
      ctx.addOne((name,substitute(ctx)(body)))
      body
    case Var(target) => get(ctx)(target) match
      case Some(Var(_)) => throw new Exception(s"undefined variable $target")
      case Some(value) => value
      case None => throw new Exception(s"undefined variable $target")
    case Combination(oper::args) =>
      oper.eval(ctx) match
        case apply: Apply => apply.apply(ctx,args)
        case func => func
    case expr => expr
}

def parse(input:String) = LispParser.program.parse(input)

def printCtx(ctx:Context):Unit = {
  ctx.filter((n,_) => !baseContext.exists(_._1 == n)).foreach(println)
}

def eval(input:String,ctx:Context = new Context) = parse(input).map(_.map(x => {x.eval(ctx)})) match
  case Ok(x::Nil) => x
  case Ok(value) => value
  case e:Err => e.error

val baseContext = ListBuffer(
  "+" -> BuiltIn(args => ctx =>
    Num(args.map{
      case Num(value) => value
      case x =>
        x.eval(ctx) match
        case Num(value) => value
        case _ => throw new Exception("invalid argument")
    }.sum)
  ),
  "-" -> BuiltIn(args => ctx => {
    val values = args.map {
      case Num(value) => value
      case x => x.eval(ctx) match
        case Num(value) => value
        case _ => throw new Exception("invalid argument")
    }
    values match
      case Nil => throw new Exception("invalid argument")
      case head::Nil => Num(-head)
      case head::tail => Num(tail.foldLeft(head)(_ - _))
  }),
  "*" -> BuiltIn(args => ctx => {
    val values = args.map {
      case Num(value) => value
      case x => x.eval(ctx) match
        case Num(value) => value
        case _ => throw new Exception("invalid argument")
    }
    Num(values.product)
  }),
  "/" -> BuiltIn(args => ctx => {
    val values = args.map {
      case Num(value) => value
      case x => x.eval(ctx) match
        case Num(value) => value
        case _ => throw new Exception("invalid argument")
    }
    values match
      case Nil => throw new Exception("invalid argument")
      case head::Nil => Num(1 / head)
      case head::tail => Num(tail.foldLeft(head)(_ / _))
  }),
  "=" -> BuiltIn(args => ctx => {
    if args.map(_.eval(ctx)).forall(_ == args.head.eval(ctx)) then Num(1) else Num(0)
  }),
  "<" -> BuiltIn(args => ctx => {
    val values = args.map {
      case Num(value) => value
      case x => x.eval(ctx) match
        case Num(value) => value
        case _ => throw new Exception("invalid argument")
    }
    val a::tail = values
    if tail.forall(a > _) then Num(1) else Num(0)
  }),
  ">" -> BuiltIn(args => ctx => {
    val values = args.map {
      case Num(value) => value
      case x => x.eval(ctx) match
        case Num(value) => value
        case _ => throw new Exception("invalid argument")
    }
    val a::tail = values
    if tail.forall(a < _) then Num(1) else Num(0)
  }),
  "square" -> BuiltIn(args => ctx => {
    val List(Num(value)) = args
    Num(value * value)
  }),
  "if" -> BuiltIn(args => ctx => {
    if args.length != 3 then throw new Exception("invalid argument")
    val List(cond,thenExpr,elseExpr) = args
    cond.eval(ctx) match
      case Num(1) =>thenExpr.eval(ctx)
      case _ => elseExpr.eval(ctx)
  })
)

def listOf[A](elems:A*): ListBuffer[A] = ListBuffer(elems:_*)

def contextOf(elems:(String,Expr)*): Context =  baseContext ++ ListBuffer(elems:_*)

def printFmt(any: Any): Unit = any match
  case elems:List[_] => elems.foreach(printFmt)
  case _ => println(any.toString)

@main
def main(): Unit = {
  val ctx = contextOf()
  val pgm:String =
    """
      | (define (abs x) (if (< x 0)
      |                     (- x)
      |                     x))
      | (define (average x y) (/ (+ x y) 2))
      | (define (improve guess x)
      |     (average guess (/ x guess)))
      | (define (enough guess x)
      |     (< (abs (- (square guess) x)) 0.0001))
      | (define (try guess x)
      |     (if (enough guess x)
      |         guess
      |         (try (improve guess x) x)))
      | (define (sqrt x) (try 1 x))
      | (sqrt 100)
      |""".stripMargin
  printFmt(eval(pgm,ctx))
}