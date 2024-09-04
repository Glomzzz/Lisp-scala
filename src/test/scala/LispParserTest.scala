package com.skillw

import org.scalatest.funsuite.AnyFunSuiteLike
import lisp.process.LispParser.*
import lisp.util.*

import _root_.com.skillw.lisp.term.Elem

class LispParserTest extends AnyFunSuiteLike {



  test("testWhitespace") {
    whitespace.parse("  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
  }

  test("testComment") {
    comment.parse("#comment\n  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
    (comment thenSkip comment).parse("#comment\n#comment\n  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
  }

  test("testBlank"){
    blank.parse("  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
    blank.parse("  #comment\n  ") match {
      case Ok(value) =>
      case e:Err => e.error
    }
    (blank skipThen elem).parse("  #comment\n123123 ") match {
      case Ok(value) => assertResult(Elem.num(123123))(value)
      case e:Err => e.error
    }
  }

  test("testDigits") {
    digits.parse("123") match {
      case Ok(value) => assertResult("123")(value)
      case e:Err => e.error
    }
    digits.parse("0001234  ") match {
      case Ok(value) => assertResult("0001234")(value)
      case e:Err => e.error
    }
    digits.parse(" 0001234  ") match {
      case Ok(value) => assert(false)
      case e:Err =>
    }

  }

  test("testNat") {
    nat.parse("123") match {
      case Ok(value) => assertResult(123)(value)
      case e:Err => e.error
    }
    nat.parse("0001234  ") match {
      case Ok(value) => assertResult(1234)(value)
      case e:Err => e.error
    }
  }

  test("testReal") {
    real.parse("123.123") match {
      case Ok(value) => assertResult(123.123)(value)
      case e:Err => e.error
    }
    real.parse("123.1") match {
      case Ok(value) => assertResult(123.1)(value)
      case e:Err => e.error
    }
    real.parse("123") match {
      case Ok(value) => assertResult(123)(value)
      case e:Err => e.error
    }
    real.parse("000123") match {
      case Ok(value) => assertResult(123)(value)
      case e:Err => e.error
    }
    real.parse("000123.000123") match {
      case Ok(value) => assertResult(123.000123)(value)
      case e:Err => e.error
    }

  }

  test("testName") {
    name.parse("1aBc") match {
      case Ok(value) => assert(false)
      case e:Err =>
    }
    name.parse("!aBc") match {
      case Ok(value) => assertResult("!abc")(value)
      case e:Err => e.error
    }
    (name skipThen blank skipThen name).parse("abc asddas  ") match {
      case Ok(value) => assertResult("asddas")(value)
      case e:Err => e.error
    }

  }

  test("testLeftParen") {
    leftParen.parse("(") match {
      case Ok(value) => assertResult('(')(value)
      case e:Err => e.error
    }
  }

  test("testRightParen") {
    rightParen.parse(")") match {
      case Ok(value) => assertResult(')')(value)
      case e:Err => e.error
    }
  }

  // Program

  test("testNumber") {
    number.parse("123") match {
      case Ok(value) => assertResult(Elem.num(123))(value)
      case e:Err => e.error
    }
  }

  test("testVariable") {
    variable.parse("aBc!") match {
      case Ok(value) => assertResult(Elem.variable("abc!"))(value)
      case e:Err => e.error
    }
  }

  test("testCombination") {
    com.parse("(abc 123)") match {
      case Ok(value) => assertResult(Elem.com(Elem.variable("abc"), Elem.num(123)))(value)
      case e:Err => e.error
    }
  }

  test("testElem") {
    elem.parse("123") match {
      case Ok(value) => assertResult(Elem.num(123))(value)
      case e:Err => e.error
    }
    elem.parse("abc") match {
      case Ok(value) => assertResult(Elem.variable("abc"))(value)
      case e:Err => e.error
    }
  }

  test("testProgram") {
    program.parse(" (abc 123)") match {
      case Ok(value) => assertResult(List(Elem.com(Elem.variable("abc"), Elem.num(123))))(value)
      case e:Err => e.error
    }
    program.parse("abc 123 (abc (abc (abc (abc 123))))  ") match {
      case Ok(value) => assertResult(List(Elem.variable("abc"), Elem.num(123.0), Elem.com(Elem.variable("abc"), Elem.com(Elem.variable("abc"), Elem.com(Elem.variable("abc"), Elem.com(Elem.variable("abc"), Elem.num(123.0)))))))(value)
      case e:Err => e.error
    }

    val pgm =
      """
        | (define (square x)
        |  (* x x))
        |
        | (a (- 2 1))
        | (b (- 3 1))
        |
        |""".stripMargin

    program.parse(pgm) match {
      case Ok(value) => assertResult(List(Elem.com(Elem.variable("define"), Elem.com(Elem.variable("square"), Elem.variable("x")), Elem.com(Elem.variable("*"), Elem.variable("x"), Elem.variable("x"))), Elem.com(Elem.variable("a"), Elem.com(Elem.variable("-"), Elem.num(2.0), Elem.num(1.0))), Elem.com(Elem.variable("b"), Elem.com(Elem.variable("-"), Elem.num(3.0), Elem.num(1.0)))))(value)
      case e:Err =>  e.error
    }
  }

}

