package com.skillw

import ParserPure.*
import Expr.*

type ParserPureState = (String, Int)

class ParserPure[+A](val parser: ParserPureState => Option[(A, ParserPureState)]) {

  def run(state: ParserPureState): Option[(A, ParserPureState)] = parser(state)

  def parse(input: String): Option[A] = {
    val state = (input, 0)
    run(state).map(_._1)
  }


  /**
   * ParserPure[A] => (A => ParserPure[B]) => ParserPure[B]
   *
   * 其中  A => ParserPure[B] 完成ParserPure[B]的生成
   *
   * 直观解释:
   * state =>
   * val (a, newState) = ParserPure[A].run(state)
   * val parserB = map(a)
   * parserB.run(newState)
   *
   * @param map 变换函数 A => ParserPure[B]
   * @tparam B 变换后的类型
   * @return
   */
  def flatMap[B](map: A => ParserPure[B]): ParserPure[B]
  = ParserPure(state => {
    // 1. 使用当前ParserPure解析输入
    parser(state) match {
      // 2. 如果解析成功，获取解析结果a和新的状态newState
      case Some((a, newState))
        // 3. 使用map(a)创建新的解析器ParserPure[B]
        // 4. 使用新的解析器ParserPure[B]解析newState
      => map(a).run(newState)
      case None => None
    }
  })

  /**
   * ParserPure[A] => (A => B) => ParserPure[B]
   *
   * 其中
   * 通过 A => B 完成B的生成
   * 通过 pure: B => ParserPure[B] 完成ParserPure[B]的生成
   *
   *
   * 直观解释:
   * state =>
   * val (a, newState) = ParserPure[A].run(state)
   * val parserB = pure(map(a))
   * parserB.run(newState)
   *
   * @param map 变换函数
   * @tparam B 变换后的类型
   * @return ParserPure[B]
   */
  def map[B](map: A => B): ParserPure[B] = flatMap(a => pure(map(a)))

  /**
   * 用于将两个ParserPure的结果组合成一个新的ParserPure
   *
   * @param that    另一个ParserPure
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return ParserPure[C]
   */
  def combine[B, C](that: ParserPure[B])
                   (combine: (A, B) => C): ParserPure[C]
  = flatMap(a => that.map(b => combine(a, b)))


  /**
   * 用于将两个ParserPure的结果组合成一个新的ParserPure
   *
   * @param that    另一个ParserPure
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return ParserPure[C]
   */
  def maybe[B, C](that: ParserPure[B])
                 (combine: (A, Option[B]) => C): ParserPure[C]
  = flatMap(a => ParserPure(state => {
    that.run(state) match {
      case Some((b, newState)) => Some((combine(a, Some(b)), newState))
      case None => Some((combine(a, None), state))
    }
  }))

  /**
   * 用于将两个ParserPure的结果组合成一个新的ParserPure
   *
   * @param that    另一个ParserPure
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return ParserPure[C]
   */
  def flatCombine[B, C](that: ParserPure[B])
                       (combine: (A, B) => ParserPure[C]): ParserPure[C]
  = flatMap(a => that.flatMap(b => combine(a, b)))

  /**
   * 与另一个ParserPure组合，返回自己结果
   *
   * @param that 另一个ParserPure
   * @tparam B 另一个ParserPure的类型
   * @return ParserPure[A]
   */
  def thenSkip[B](that: ParserPure[B]): ParserPure[A] = combine(that)((a, _) => a)

  /**
   * 与另一个ParserPure组合，返回另一个ParserPure的结果
   *
   * @param that 另一个ParserPure
   * @tparam B 另一个ParserPure的类型
   * @return ParserPure[B]
   */
  def skipThen[B](that: ParserPure[B]): ParserPure[B] = combine(that)((_, b) => b)

  /**
   * 如果当前ParserPure[A]解析失败，尝试使用另一个ParserPure[B]解析
   *
   * @param that 另一个ParserPure[B]
   * @return ParserPure[Any]
   */
  def or[B](that: ParserPure[B]) = ParserPure(state => {
    parser(state) match {
      case None => that.parser(state)
      case result => result
    }
  })


  def all: ParserPure[List[A]] = {
    def loop(acc: List[A]): ParserPure[List[A]] = ParserPure(state => {
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

  def many: ParserPure[List[A]] = {

    def loop(acc: List[A]): ParserPure[List[A]] = flatMap(a => {
      many.map(as => a :: as)
    }).or(pure(acc))

    loop(List())
  }

  def some: ParserPure[List[A]] = {
    combine(many)((a, as) => a :: as)
  }

}

//noinspection ScalaWeakerAccess,TypeAnnotation
object ParserPure {
  def pure[A](a: A): ParserPure[A] = ParserPure(state => Some((a, state)))

  def fail[A]: ParserPure[A] = ParserPure(_ => None)

  val id = ParserPure(state => {
    val (str, index) = state
    if (index < str.length)
      Some((str(index), (str, index + 1)))
    else None
  })

  def pred(pred: Char => Boolean): ParserPure[Char] =
    id.flatMap(c => if (pred(c)) pure(c) else fail)

  def char(c: Char): ParserPure[Char] = pred(_ == c)

  def digit = pred(c => '0' <= c && c <= '9').map(_ - '0')

  def nat = digit.some.map(_.foldLeft(0)(_ * 10 + _))

  def int = char('-').skipThen(nat).map(-_) or nat

  def zero = char('0').map(_ => 0)

  def real = int.combine(char('.').skipThen((zero.many combine nat)((zeros,nat)=>{
    if nat == 0 then 0 else {
      val length = zeros.length + nat.toString.length
      nat / math.pow(10, length)
    }
  })))((fixed, decimal) => fixed.toDouble + decimal) or int.map(_.toDouble)

}

//noinspection ScalaWeakerAccess,TypeAnnotation
object LispParserPure{

  def white = pred(_.isWhitespace).many

  def nameStart = pred(c => c.isLetter || c == '_')
  def namePart = pred(c => c.isLetter || c.isDigit || c == '_').many

  def operator = pred(c => c == '+' || c == '-' || c == '*' || c == '/' || c == '=' || c == '<' || c == '>')

  def name = white skipThen (nameStart combine namePart ) (_ :: _) .map(_.mkString.toLowerCase) thenSkip white

  def str = white skipThen char('"') skipThen pred(_ != '"').some thenSkip char('"') map(_.mkString) thenSkip white

  def literal = white skipThen (real or str).map {
    case str: String => Str(str)
    case num: Double => Num(num)
  } thenSkip white

  def leftParen = white skipThen char('(') thenSkip white

  def rightParen = white skipThen char(')') thenSkip white

  def params : ParserPure[List[String]] =
    (leftParen skipThen name.many thenSkip rightParen) or (name map (name => List(name)))

  def lambda = (params combine combinator) (Lam.apply)

  //noinspection NotImplementedCode
  def define:ParserPure[Expr] =  (params combine combinator) ((list, body) => {
    list match
      case name::Nil => Def(name, body)
      case name::params => Def(name, Lam(params, body))
      case _ => throw new Exception("define no name error")
  })


  def call:ParserPure[Expr] = (name or operator).flatMap {
    case "lambda" => lambda
    case "define" => define
    case name:String => pure(Var(name))
    case operator:Char => pure(Var(operator.toString))
  }

  private def combinator_ = ParserPure(state => combinator.run(state))

  def combinator: ParserPure[Expr] =
    (leftParen skipThen combinator_.many.map({
      case Nil => Combination(List())
      case x::Nil => x
      case exprs => Combination(exprs)
}
    ) thenSkip rightParen) or call or literal


  def program = combinator.all
}