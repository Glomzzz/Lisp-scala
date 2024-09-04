# Lisp-Scala

## Intro

最近刚开始上 [SICP(1985年的课)](https://www.youtube.com/watch?v=-J_xL4IGhJA&list=PLE18841CABEA24090) 先整个 Lisp 解释器当练习环境。

顺便用**Scala3**练手,  
整了个自认为好用的，带错误报告的 [ParserCombinator](https://github.com/Glomzzz/ParserCombinator)

## Usage

你大概只会用到 `com.skillw.lisp.Lisp` 与 `com.skillw.lisp.scope.BuiltIn` 这两个类

### Lisp

这个类里封装了eval函数，你可以通过创建一个Lisp对象来执行它:

Lisp的构造函数接受一些执行选项:
- ShowElems: 显示原始的语法树
- ShowExprs: 显示脱糖后的语法树
- ShowEvals: 求值过程
- ShowContext: 显示上下文


```scala 3
import lisp.LispOption.*
import lisp.Lisp

val input =
  """
        | (define (add x y) (+ x y))
        | (define x (add 1 26))
        | (cond  ((= x 3) (print "x  =  3"))
        |        ((= x 4) (print "x  =  4"))
        |        ((= x 5) (print "x  =  5"))
        |        (  _     (print "x  =  ?"))
        | )
        |
        |""".stripMargin

Lisp(
  ShowElems,
  ShowExprs,
  ShowEvals,
  ShowContext,
).eval(input)
```
你可以通过 Expr#toJava 来将结果转换为Java对象

控制台输出:

```
Elems:
(define (add x y) (+ x y))
(define x (add 1.0 26.0))
(cond ((= x 3.0) (print "x  =  3")) ((= x 4.0) (print "x  =  4")) ((= x 5.0) (print "x  =  5")) (_ (print "x  =  ?")))

Exprs:
(define add (lambda (x y) (+ x y)))
(define x (add 1.0 26.0))
(cond ((= x 3.0) (print x  =  3)) ((= x 4.0) (print x  =  4)) ((= x 5.0) (print x  =  5)) (_ (print x  =  ?)))

Evaluating:

 >> (define add (lambda (x y) (+ x y)))
 => (lambda (x y) (+ x y))


 >> (define x (add 1.0 26.0))
       >      add -> (lambda (x y) (+ x y))
 => 27.0

x  =  ?

 >> (cond ((= x 3.0) (print x  =  3)) ((= x 4.0) (print x  =  4)) ((= x 5.0) (print x  =  5)) (_ (print x  =  ?)))
       >      x -> 27.0
 => x  =  ?
```

当然，你可以尝试更复杂的Lisp:

- 你可以通过向Context中传入一个值来完成与Lisp的交互
- 你可以通过调用 Expr#toJava 来将结果转换为Java对象

```scala 3
import lisp.LispOption.*
import lisp.Lisp
import lisp.scope.Context
import lisp.term.Expr.*

    val input: String =
  """
    | (define (abs x) (if (< x 0)
    |                     (- x)
    |                     x))
    | (define (average x y) (/ (+ x y) 2))
    | (define (improve guess x)
    |     (average guess (/ x guess)))
    | (define (square x) (* x x))
    | (define (enough guess x)
    |     (< (abs (- (square guess) x)) 0.0001))
    | (define (try guess x)
    |     (if (enough guess x)
    |         guess
    |         (try (improve guess x) x)))
    | (define (sqrt x) (try 1 x))
    | (sqrt 10)
    | (sqrt 100)
    | (sqrt 1000)
    | (sqrt m)
    |""".stripMargin

    val ctx = Context().add("m", Num(10000))
    val result = Lisp(
      ShowEvals
    ).eval(input,ctx).toJava
    
    println(result)
```

控制台输出:

```
Evaluating:

// 省略函数的Eval过程

 >> (sqrt 10.0)
 => 3.162277665175675


 >> (sqrt 100.0)
 => 10.000000000139897


 >> (sqrt 1000.0)
 => 31.622776601684336


 >> (sqrt m)
 => 100.00000025490743

100.00000025490743
```

### BuiltIn

这个类提供向Lisp环境中添加内建函数的方法

在 [lisp.scope.BuiltInContext](https://github.com/Glomzzz/Lisp-scala/blob/master/src/main/scala/lisp/scope/BuiltIn.scala) 中已经有了一些例子

// TODO

## Todo-List

- [x] 更多内建函数
- [x] 完善错误提示
- [-] 完善与Java的交互
- [-] Maybe 简单的 REPL
- [-] Maybe 简单的终端界面编辑器


