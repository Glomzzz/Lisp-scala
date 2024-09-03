package com.skillw

import org.scalatest.funsuite.AnyFunSuiteLike

class ParserTest extends AnyFunSuiteLike {

  test("testNat") {
    Parser.nat.parse("123") match {
      case Ok(value) => assert(value == 123)
      case e:Err => e.error
    }
  }

  test("testReal") {
    Parser.real.parse("123 456") match {
      case Ok(value) => assert(value == 123)
      case e:Err => e.error
    }
    Parser.real.parse("123.456") match {
      case Ok(value) => assert(value == 123.456)
      case e:Err => e.error
    }
    Parser.real.parse("0.456") match {
      case Ok(value) => assert(value == 0.456)
      case e:Err => e.error
    }
    Parser.real.parse("0.000001") match {
      case Ok(value) => assert(value == 0.000001)
      case e:Err => e.error
    }
  }

  test("testPure") {
    Parser.pure(1).parse("123123") match {
      case Ok(value) => assert(value == 1)
      case e:Err => e.error
    }
  }

  test("testInt") {
    Parser.int.parse("-123") match {
      case Ok(value) => assert(value == -123)
      case e:Err => e.error
    }
  }

//  test("testFail") {
//    Parser.none.parse("hello world") match {
//      case Ok(value) => assert(false)
//      case None => assert(true)
//    }
//  }

  test("testPred") {
    Parser.pred((c: Char) => c.isDigit).parse("123") match {
      case Ok(value) => assert(value == '1')
      case e:Err => e.error
    }
  }

  test("testChar") {
    Parser.char('1').parse("123") match {
      case Ok(value) => assert(value == '1')
      case e:Err => e.error
    }
  }


  test("testId") {
    Parser.id.parse("hello world") match {
      case Ok(value) => assert(value == 'h')
      case e:Err => e.error
    }
  }

  test("testDigit") {
    Parser.digit.parse("123") match {
      case Ok(value) => assert(value == 1)
      case e:Err => e.error
    }
  }

}
