package com.skillw


import Parser.*
import Expr.*

type ParserState = (String, Int)

case class Ok[T](value: T) extends Result[T]

case class Err(errs:List[(String, ParserState)]) extends Result[Nothing]{
  def combine(that: Err):Err = {
    val errs = List(this.errs: _*)
    that.errs.filter({
        case (str, state) => !errs.exists({
          case (str2, state2) => str == str2 || state == state2
        })
      }).foldLeft(errs)((acc, err) => err :: acc)
    Err(errs)
  }
  def errorMsg =
    s"""
      | Error occurred while parsing:
      | ${errs.map((msg,state)=>errorMessage(state,msg)).mkString("\n")}
      |""".stripMargin
  def error = throw Exception(errorMsg)
}

def errorMessage(state:ParserState,msg:String): String = {
  val (input, index) = state
  val before = input.take(index+1)
  val line = before.count(_ == '\n')+1
//  before.reverse.foreach( x =>
//    println(s"char: '$x'")
//  )
  val column = before.reverse.takeWhile(_ != '\n').length + 1
  s"""
     | $msg at line $line column $column ; index $index :
     | ${input.split('\n').lift(line - 1).getOrElse("")}
     | ${" " * (column - 1)}^
     |""".stripMargin
}

sealed trait Result[+T]{
  def map[B](f:T=>B):Result[B] = this match {
    case Ok(value) => Ok(f(value))
    case e:Err => e
  }

  def unwarp:T = this match {
    case Ok(value) => value
    case e:Err => e.error
  }

  def option:Option[T] = this match {
    case Ok(value) => Some(value)
    case _ => scala.None
  }
}

class Parser[+A](val parser: ParserState => Result[(A, ParserState)]) {

  def run(state: ParserState)= parser(state)

  def parse(input: String): Result[A] = {
    val state = (input, 0)
    run(state).map(_._1)
  }


  /**
   * Parser[A] => (A => Parser[B]) => Parser[B]
   *
   * 其中  A => Parser[B] 完成Parser[B]的生成
   *
   * 直观解释:
   * state =>
   * val (a, newState) = Parser[A].run(state)
   * val parserB = map(a)
   * parserB.run(newState)
   *
   * @param map 变换函数 A => Parser[B]
   * @tparam B 变换后的类型
   * @return
   */
  def flatMap[B](map: A => Parser[B]): Parser[B]
  = Parser(state => {
    // 1. 使用当前Parser解析输入
    parser(state) match {
      // 2. 如果解析成功，获取解析结果a和新的状态newState
      case Ok((a, newState))
        // 3. 使用map(a)创建新的解析器Parser[B]
        // 4. 使用新的解析器Parser[B]解析newState
      => map(a).run(newState)
      case err:Err => err
    }
  })

  /**
   * Parser[A] => (A => B) => Parser[B]
   *
   * 其中
   * 通过 A => B 完成B的生成
   * 通过 pure: B => Parser[B] 完成Parser[B]的生成
   *
   *
   * 直观解释:
   * state =>
   * val (a, newState) = Parser[A].run(state)
   * val parserB = pure(map(a))
   * parserB.run(newState)
   *
   * @param map 变换函数
   * @tparam B 变换后的类型
   * @return Parser[B]
   */
  def map[B](map: A => B): Parser[B] = flatMap(a => pure(map(a)))

  /**
   * 用于将两个Parser的结果组合成一个新的Parser
   *
   * @param that    另一个Parser
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return Parser[C]
   */
  def combine[B, C](that: Parser[B])
                   (combine: (A, B) => C): Parser[C]
  = flatMap(a => that.map(b => combine(a, b)))

  /**
   * 用于将两个Parser的结果组合成一个新的Parser
   *
   * @param that    另一个Parser
   * @param combine 组合函数
   * @tparam B 另一个
   * @tparam C 组合后的类型
   * @return Parser[C]
   */
  def flatCombine[B, C](that: Parser[B])
                       (combine: (A, B) => Parser[C]): Parser[C]
  = flatMap(a => that.flatMap(b => combine(a, b)))

  /**
   * 与另一个Parser组合，返回自己结果
   *
   * @param that 另一个Parser
   * @tparam B 另一个Parser的类型
   * @return Parser[A]
   */
  def thenSkip[B](that: Parser[B]): Parser[A] = combine(that)((a, _) => a)

  /**
   * 与另一个Parser组合，返回另一个Parser的结果
   *
   * @param that 另一个Parser
   * @tparam B 另一个Parser的类型
   * @return Parser[B]
   */
  def skipThen[B](that: Parser[B]): Parser[B] = combine(that)((_, b) => b)

  /**
   * 如果当前Parser[A]解析失败，尝试使用另一个Parser[B]解析
   *
   * @param that 另一个Parser[B]
   * @return Parser[Any]
   */
  def or[B](that: Parser[B]) = Parser(state => {
    parser(state) match {
      case e:Err => that.parser(state) match {
        case e2: Err => e.combine(e2)
        case result => result
      }
      case result => result
    }
  })


  def all: Parser[List[A]] = {
    def loop(acc: List[A]): Parser[List[A]] = Parser(state => {
      parser(state) match {
        case Ok((a, newState))
        => if (newState._2 < newState._1.length)
              all.map(as => a :: as).run(newState)
            else Ok((a :: acc, newState))
        case e:Err => e
      }
    })


    loop(List())
  }

  def many: Parser[List[A]] = {

    def loop(acc: List[A]): Parser[List[A]] = flatMap(a => {
      many.map(as => a :: as)
    }).or(pure(acc))

    loop(List())
  }

  def some: Parser[List[A]] = {
    combine(many)((a, as) => a :: as)
  }

}

//noinspection ScalaWeakerAccess,TypeAnnotation
object Parser {
  def pure[A](a: A): Parser[A] = Parser(state => Ok((a, state)))

  def fail[A](msg: String): Parser[A] = Parser(state => Err(List((msg, (state._1, state._2+1)))))

  val id = Parser(state => {
    val (str, index) = state
    if (index < str.length)
      Ok((str(index), (str, index + 1)))
    else Err(List(("unexpected end of input", (str,index))))
  })

  def pred(pred: Char => Boolean): Parser[Char] =
    id.flatMap(c => if (pred(c)) pure(c) else fail(s"unexpected $c"))

  def char(expect: Char): Parser[Char] = id.flatMap(actual => if (expect == actual) pure(expect) else fail(s"Expected $expect but got $actual"))

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
object LispParser{

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

  def params : Parser[List[String]] =
    (leftParen skipThen name.many thenSkip rightParen) or (name map (name => List(name)))

  def lambda = (params combine combinator) (Lam.apply)

  //noinspection NotImplementedCode
  def define:Parser[Expr] =  (params combine combinator) ((list, body) => {
    list match
      case name::Nil => Def(name, body)
      case name::params => Def(name, Lam(params, body))
      case _ => throw new Exception("define no name error")
  })


  def call:Parser[Expr] = (name or operator).flatMap {
    case "lambda" => lambda
    case "define" => define
    case name:String => pure(Var(name))
    case operator:Char => pure(Var(operator.toString))
  }

  private def combinator_ = Parser(state => combinator.run(state))

  def combinator: Parser[Expr] =
    (leftParen skipThen combinator_.many.map({
      case Nil => Combination(List())
      case x::Nil => x
      case exprs => Combination(exprs)
}
    ) thenSkip rightParen) or call or literal


  def program = combinator.all
}