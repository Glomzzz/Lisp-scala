package com.skillw

import org.scalatest.funsuite.AnyFunSuiteLike
import lisp.LispOption.*

import lisp.Lisp

class LispTest extends AnyFunSuiteLike {

  test("cond") {
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
      ShowAtoms,
      ShowExprs,
      ShowContext,
      ShowEvals
    ).eval(input).toJava
    
    assertResult("x  =  ?")(result)
  }

  test("sqrt") {
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
        | (print (sqrt 10))
        |""".stripMargin

    val result = Lisp(
      ShowAtoms,
      ShowExprs,
      ShowContext,
      ShowEvals
    ).eval(input).toJava

    assertResult(3.162277665175675)(result)
  }

  test("desugarError"){
    try {
      val input =
        """
          | (define () (+ x y))
          | (define  (add 1 26))
          |
          |""".stripMargin

      Lisp(ShowAtoms).eval(input)
      assert(false)
    }catch
      case e: Exception => e.printStackTrace()
  }

  test("builtInError"){
    try {
      val input =
        """
          | (+ 1 2 3 4 5)
          | (-)
          |
          |""".stripMargin

      Lisp(ShowAtoms).eval(input)
      assert(false)
    }catch
      case e: Exception => e.printStackTrace()
  }

}