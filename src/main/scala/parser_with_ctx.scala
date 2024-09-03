package com.skillw


import ParserCtx.*
import Expr.*

type PContext = List[(String, Expr)]

type ParserStateCtx = (String, Int, PContext)

/**
 * Clearly a wrong decision to add context to the parser state.
 *
 */

class ParserCtx[+A](val parser: ParserStateCtx => Option[(A, ParserStateCtx)]) {

  def run(state: ParserStateCtx): Option[(A, ParserStateCtx)] = parser(state)

  def parse(input: String, ctx: PContext = List()): Option[A] = {
    val state = (input, 0, ctx)
    run(state).map(_._1)
  }


  /**
   * ParserCtx[A] => (A => ParserCtx[B]) => ParserCtx[B]
   *
   * 其中  A => ParserCtx[B] 完成ParserCtx[B]的生成
   *
   * 直观解释:
   * state =>
   * val (a, newState) = ParserCtx[A].run(state)
   * val parserB = map(a)
   * parserB.run(newState)
   *
   * @param map 变换函数 A => ParserCtx[B]
   * @tparam B 变换后的类型
   * @return
   */
  def flatMap[B](map: A => ParserCtx[B]): ParserCtx[B]
  = ParserCtx(state => {
    // 1. 使用当前Parser解析输入
    parser(state) match {
      // 2. 如果解析成功，获取解析结果a和新的状态newState
      case Some((a, newState))
        // 3. 使用map(a)创建新的解析器ParserCtx[B]
        // 4. 使用新的解析器ParserCtx[B]解析newState
      => map(a).run(newState)
      case None => None
    }
  })

  /**
   * ParserCtx[A] => (A => B) => ParserCtx[B]
   *
   * 其中
   * 通过 A => B 完成B的生成
   * 通过 pure: B => ParserCtx[B] 完成ParserCtx[B]的生成
   *
   *
   * 直观解释:
   * state =>
   * val (a, newState) = ParserCtx[A].run(state)
   * val parserB = pure(map(a))
   * parserB.run(newState)
   *
   * @param map 变换函数
   * @tparam B 变换后的类型
   * @return ParserCtx[B]
   */
  def map[B](map: A => B): ParserCtx[B] = flatMap(a => pure(map(a)))


  /**
   * 用于将两个Parser的结果组合成一个新的Parser
   * 状态内的Context不会改变
   *
   * @param that    另一个Parser
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return ParserCtx[C]
   */
  def recover[B, C](that: ParserCtx[B])
                   (combine: (A, B, PContext) => (C, PContext)): ParserCtx[C]
  = ParserCtx((input, index, ctx) => {
    run(input, index, ctx) match {
      case Some((a, (_, index, bodyCtx))) =>
        that.run(input, index, bodyCtx) match {
          case Some((b, (_, index, bodyCtx))) =>
            val (c, newCtx) = combine(a, b, ctx)
            Some((c, (input, index, newCtx)))
          case None => None
        }
      case _ => None
    }
  })

  /**
   * 用于将两个Parser的结果组合成一个新的Parser
   *
   * @param that    另一个Parser
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return ParserCtx[C]
   */
  def combine[B, C](that: ParserCtx[B])
                   (combine: (A, B) => C): ParserCtx[C]
  = flatMap(a => that.map(b => combine(a, b)))


  /**
   * 用于将两个Parser的结果组合成一个新的Parser
   *
   * @param that    另一个Parser
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return ParserCtx[C]
   */
  def maybe[B, C](that: ParserCtx[B])
                 (combine: (A, Option[B]) => C): ParserCtx[C]
  = flatMap(a => ParserCtx(state => {
    that.run(state) match {
      case Some((b, newState)) => Some((combine(a, Some(b)), newState))
      case None => Some((combine(a, None), state))
    }
  }))

  /**
   * 用于将两个Parser的结果组合成一个新的Parser
   *
   * @param that    另一个Parser
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return ParserCtx[C]
   */
  def flatCombine[B, C](that: ParserCtx[B])
                       (combine: (A, B) => ParserCtx[C]): ParserCtx[C]
  = flatMap(a => that.flatMap(b => combine(a, b)))

  /**
   * 与另一个Parser组合，返回自己结果
   *
   * @param that 另一个Parser
   * @tparam B 另一个Parser的类型
   * @return ParserCtx[A]
   */
  def thenSkip[B](that: ParserCtx[B]): ParserCtx[A] = combine(that)((a, _) => a)

  /**
   * 与另一个Parser组合，返回另一个Parser的结果
   *
   * @param that 另一个Parser
   * @tparam B 另一个Parser的类型
   * @return ParserCtx[B]
   */
  def skipThen[B](that: ParserCtx[B]): ParserCtx[B] = combine(that)((_, b) => b)

  /**
   * 如果当前ParserCtx[A]解析失败，尝试使用另一个ParserCtx[B]解析
   *
   * @param that 另一个ParserCtx[B]
   * @return ParserCtx[Any]
   */
  def or[B](that: ParserCtx[B]) = ParserCtx(state => {
    parser(state) match {
      case None => that.parser(state)
      case result => result
    }
  })


  def all: ParserCtx[List[A]] = {
    def loop(acc: List[A]): ParserCtx[List[A]] = ParserCtx(state => {
      parser(state) match {
        case Some((a, newState))
        => if (newState._2 < newState._1.length)
          all.map(as => a :: as).run(newState)
        else Some((a :: acc, newState))
        case None => None
      }
    })


    loop(List())
  }

  def many: ParserCtx[List[A]] = {

    def loop(acc: List[A]): ParserCtx[List[A]] = flatMap(a => {
      many.map(as => a :: as)
    }).or(pure(acc))

    loop(List())
  }

  def some: ParserCtx[List[A]] = {
    combine(many)((a, as) => a :: as)
  }

}

//noinspection ScalaWeakerAccess,TypeAnnotation
object ParserCtx {
  def pure[A](a: A): ParserCtx[A] = ParserCtx(state => Some((a, state)))

  def fail[A]: ParserCtx[A] = ParserCtx(_ => None)

  val id = ParserCtx(state => {
    val (str, index, ctx) = state
    if (index < str.length)
      Some((str(index), (str, index + 1, ctx)))
    else None
  })

  def pred(pred: Char => Boolean): ParserCtx[Char] =
    id.flatMap(c => if (pred(c)) pure(c) else fail)

  def char(c: Char): ParserCtx[Char] = pred(_ == c)

  def digit = pred(c => '0' <= c && c <= '9').map(_ - '0')

  def nat = digit.some.map(_.foldLeft(0)(_ * 10 + _))

  def int = char('-').skipThen(nat).map(-_) or nat

  def real = int.combine(char('.').skipThen(nat))((fixed, decimal) => fixed.toDouble + (decimal / Math.pow(10, decimal.toString.length))) or int.map(_.toDouble)

}

//noinspection ScalaWeakerAccess,TypeAnnotation
object LispParserWithContext{

  def white = pred(_.isWhitespace).many

  def nameStart = pred(c => c.isLetter || c == '_')
  def namePart = pred(c => c.isLetter || c.isDigit || c == '_').many

  def name = white skipThen (nameStart combine namePart ) (_ :: _) .map(_.mkString.toLowerCase) thenSkip white

  def str = white skipThen char('"') skipThen pred(_ != '"').some thenSkip char('"') map(_.mkString) thenSkip white

  def literal = white skipThen (real or str).map {
    case str: String => Str(str)
    case num: Double => Num(num)
  } thenSkip white

  def leftParen = white skipThen char('(') thenSkip white

  def rightParen = white skipThen char(')') thenSkip white

  def params : ParserCtx[List[String]] =
    (leftParen skipThen name.many thenSkip rightParen)
      .flatMap(names => ParserCtx((input,index,ctx) =>
        var bodyCtx = ctx
        for name <- names do bodyCtx = (name,Var(name)) :: bodyCtx
        Some((names,(input,index,bodyCtx)))
      )) or (name flatMap(name => ParserCtx((input,index,ctx) => Some((List(name),(input,index,(name,Var(name)) :: ctx))))))

  def lambda = (params recover combinator) ((params,body,ctx) => (Lam(params,body),ctx))

  //noinspection NotImplementedCode
  def define =  (params recover combinator) ((list, body, ctx) => {
    list match
      case name::params =>
        val lambda = Lam(params,body)
        (lambda,(name,lambda) :: ctx)
      case _ => throw new Exception("define no name error")
  })


  def call:ParserCtx[Expr] = name.flatMap {
    case "lambda" => lambda
    case "define" => define
    case name =>
      ParserCtx(state => state._3.find(_._1 == name).map((_,expr) => (expr, state)))
  }

  private def combinator_ = ParserCtx(state => combinator.run(state))

  def combinator: ParserCtx[Expr] =
    (leftParen skipThen combinator_.many.map({
      case Nil => Combination(List())
      case exprs => Combination(exprs)
    }) thenSkip rightParen) or call or literal


  def program = combinator.all
}