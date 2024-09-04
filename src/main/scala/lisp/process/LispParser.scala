package com.skillw
package lisp.process

import Parser.*
import lisp.term.Elem

//noinspection TypeAnnotation,ScalaWeakerAccess
object LispParser {
  def whitespace = pred(_.isWhitespace, "whitespace")

  def comment = char('#') skipThen pred(_ != '\n').many thenSkip char('\n')

  def blank = (comment or whitespace).many

  private def nameGen(without: Char => Boolean): Parser[Char] = pred({
    case '(' | ')' => false
    case c if without(c) => false
    case _ => true
  }, "name")

  private def nameHead = nameGen(c => c.isWhitespace || c.isDigit)

  private def namePart = nameGen(_.isWhitespace)

  def name = (nameHead and namePart.many)((head, part) => (head :: part).mkString.toLowerCase)

  def nat = digit.some.map(_.mkString.toInt)

  def digits = digit.some.map(_.mkString)

  def real = nat.ifExpect(char('.'))(digits)((fixed, decimal) => fixed + s"0.$decimal".toDouble)(pure(0))((fixed, _) => fixed.toDouble)

  def str = char('"') skipThen ((char('\\') and id())(_ + _.toString) or pred(_ != '"')).many thenSkip char('"') map (_.mkString)

  def leftParen = char('(')

  def rightParen = blank skipThen char(')')

  // Program

  def variable = name.stateMap((n, s) => Elem.Var(s.index,n))

  def number = real.stateMap((n, s) => Elem.Num(s.index,n))

  def string = str.stateMap((s, st) => Elem.Str(st.index,s))

  def elem: Parser[Elem] = string or variable or number or Parser(s => com.run(s))

  def com = (leftParen skipThen (blank skipThen elem).many thenSkip rightParen).stateMap((es, s) => Elem.Com(s.index,es:_*))

  def program = elem.all(blank)
}
