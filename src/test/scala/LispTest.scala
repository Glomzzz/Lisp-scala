package com.skillw

import org.scalatest.funsuite.AnyFunSuiteLike
import lisp.LispOption.*
import lisp.Lisp

import lisp.scope.*
import lisp.term.Expr.Num

class LispTest extends AnyFunSuiteLike {

  test("condTest") {
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
      
    val result = Lisp(
      ShowElems,
      ShowExprs,
      ShowContext,
      ShowEvals
    ).eval(input).toJava
    
    assertResult("x  =  ?")(result)
  }

  test("sqrtTest") {
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
  }

  test("desugarErrorTest"){
    try {
      val input =
        """
          | (define () (+ x y))
          | (define  (add 1 26))
          |
          |""".stripMargin

      Lisp(ShowElems).eval(input)
      assert(false)
    }catch
      case e: Exception => e.printStackTrace()
  }

  test("builtInErrorTest"){
    try {
      val input =
        """
          | (+ 1 2 3 4 5)
          | (-)
          |
          |""".stripMargin

      Lisp(ShowElems).eval(input)
      assert(false)
    }catch
      case e: Exception => e.printStackTrace()
  }

  test("builtInTest"){
    val builtIns = DefaultContext.copy
    
    val input: String =
      """
        | 
        |""".stripMargin

    val ctx = Context().add("m", Num(10000))
    val result = Lisp(
      ShowEvals
    ).eval(input, ctx).toJava

    println(result)
  }
}