package com.skillw

import org.scalatest.funsuite.AnyFunSuiteLike
import LispParser.*

import Expr.*


class LispParserTest extends AnyFunSuiteLike {

  test("testCombinator") {
    combinator.parse("(a b c)") match {
      case Ok(value) =>
        assertResult(Combination(List(Var("a"), Var("b"), Var("c"))))(value)
      case e:Err => e.error
    }

    combinator.parse("((lambda (b) b) 1)") match {
      case Ok(value) =>
        println(value)
        assertResult(Combination(List(Lam(List("b"),Var("b")),Num(1.0))))(value)
      case e:Err => e.error
    }

    combinator.parse("((define (a b) b) 1 )") match
      case Ok(value) =>
        assertResult(Combination(List(Def("a", Lam(List("b"), Var("b"))), Num(1.0))))(value)
      case e:Err => e.error

    combinator.parse("define (a f x) (f (f x))") match
      case Ok(value) =>
        println(value)
      case e:Err => e.error
  }

  test("testProgram") {
    val pgm =
      """
        | (define (a f x) (f (f x)) )
        | (define (bb x) (a a x))
        | (bb 1)
        |""".stripMargin
    program.parse(pgm) match {
      case Ok(value) =>
        println(value)
        
        assertResult(
          List(Def("a", Lam(List("f", "x"), Combination(List(Var("f"), Combination(List(Var("f"), Var("x"))))))), Def("bb", Lam(List("x"), Combination(List(Var("a"), Var("a"), Var("x"))))), Combination(List(Var("bb"), Num(1.0))))
        )(value)
      case e:Err => e.error
    }

  }

  test("testParams") {
    params.parse("(a b c)") match {
      case Ok(value) =>
        assertResult (List("a","b","c")) (value)
      case e:Err => e.error
    }
  }

  test("testLiteral") {
    literal.parse("1.0") match {
      case Ok(Num(value)) => assert(value == 1.0)
      case _ => assert(false)
    }
    literal.parse("\"hello world\"") match {
      case Ok(Str(value)) => assert(value == "hello world")
      case _ => assert(false)
    }
  }

  test("testCall") {
    call.parse("a") match {
      case Ok(value) => assertResult(Var("a"))(value)
      case e:Err => e.error
    }

    call.parse("laMbda (a b) (a b)") match {
      case Ok(value) =>
        assertResult(Lam(List("a", "b"),Combination(List(Var("a"),Var("b")))))(value)
      case e:Err => e.error
    }

    call.parse("defiNe (a b) (a b)") match {
      case Ok(value) =>
        assertResult(Def("a", Lam(List("b"), Combination(List(Var("a"), Var("b"))))))(value)
      case e:Err => e.error
    }
  }

  test("testLambda") {
    lambda.parse("(a b c) (a b c)") match {
      case Ok(value) =>
        assertResult(Lam(List("a", "b", "c"),Combination(List(Var("a"),Var("b"), Var("c")))))(value)
      case e:Err => e.error
    }
  }

  test("testDefine") {
    define.parse("(a) (a)") match {
      case Ok(value) =>
        assertResult(Def("a", Var("a")))(value)
      case e:Err => e.error
    }
    define.parse("(a) (b)") match {
      case Ok(value) =>  assertResult(Def("a", Var("b")))(value)
      case e:Err => e.error
    }

    (define.parse("(a b) (b)"), define.parse("(a b) b")) match {
      case (Ok(v1), Ok(v2)) =>
        assertResult(v1)(v2)
      case (e1:Err, e2:Err) => e1.combine(e2).error
      case (_, e:Err) => e.error
      case (e:Err, _) => e.error
    }
  }

  test("testWhite") {
    white.parse("  ") match {
      case Ok(value) => assert(value.mkString == "  ")
      case e:Err => e.error
    }
  }


  test("testStr") {
    str.parse("\"hello world\"") match {
      case Ok(value) => assert(value == "hello world")
      case e:Err => e.error
    }
  }

  test("testName") {
    name.parse("h") match {
      case Ok(value) => assert(value == "h")
      case e:Err => e.error
    }
    name.parse("hello world") match {
      case Ok(value) => assert(value == "hello")
      case e:Err => e.error
    }
    name.parse("hello1 world") match {
      case Ok(value) => assert(value == "hello1")
      case e:Err => e.error
    }
    name.parse("_1hello world") match {
      case Ok(value) => assert(value == "_1hello")
      case e:Err => e.error
    }
    name.parse("1hello world") match {
      case Ok(value) => assert(false)
      case e:Err => assert(true)
    }
  }

}
