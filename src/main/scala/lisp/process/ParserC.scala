package com.skillw
package lisp.process

import lisp.util.*



case class State(input: String, index: Int)

//noinspection, ScalaWeakerAccess
class Parser[+A](parser: State => Result[(A, State)]) extends (State => Result[(A, State)]){
  def run(state: State): Result[(A, State)] = parser(state)

  def parse(input: String): Result[A] = run(State(input, 0)) match
    case Ok((a, state)) => a.ok
    case e: Err => e


  def map[B](f: A => B): Parser[B] = Parser { state =>
    run(state) match {
      case Ok((a, newState)) => (f(a), newState).ok
      case e:Err => e
    }
  }

  def stateMap[B](f: (A, State) => B): Parser[B] = Parser { state =>
    run(state) match {
      case Ok((a, newState)) => (f(a,newState),newState).ok
      case e:Err => e
    }
  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser { state =>
    run(state) match {
      case Ok((a, newState)) => f(a).run(newState)
      case e:Err => e
    }
  }

  //noinspection TypeAnnotation
  def or[B](other: Parser[B]) = Parser { state =>
    run(state) match {
      case ok @ Ok(_) => ok
      case err1:Err => other.run(state) match {
        case ok @ Ok(_) => ok
        case err2:Err => err2
//          err1 ++ err2
      }
    }
  }

  def and[B,C] (other: Parser[B]) (f: (A,B) => C): Parser[C] = flatMap(a => other.map(b => f(a,b)))

  def many: Parser[List[A]] = Parser { state =>
    var newState = state
    var result = List.empty[A]
    val label = util.boundary.Label()
    util.boundary{
      while (true) {
        run(newState) match {
          case Ok((a, s)) =>
            newState = s
            result = a :: result
          case e:Err => util.boundary.break()
        }
      }
    }
    Ok((result.reverse, newState))
  }

  def all(blank:Parser[Any]): Parser[List[A]] = Parser { state =>
    var newState = state
    var result = List.empty[A]
    val label = util.boundary.Label()
    var error = Option.empty[Err]
    util.boundary {
      while (newState.index < newState.input.length) {
        blank.run(newState) match {
          case Ok((_, s)) => newState = s
          case e:Err =>
        }
        if (newState.index >= newState.input.length) util.boundary.break()
        run(newState) match {
          case Ok((a, s)) =>
            newState = s
            result = a :: result
          case e:Err =>
            newState = State(newState.input, e.errors.last._2+1)
            error match
              case Some(err) => error = Some(err ++ e)
              case None => error = Some(e)
        }
      }
    }
    error match {
      case Some(e) => e
      case None => Ok((result.reverse, newState))
    }
  }

  def some: Parser[List[A]] = map(List(_)).and(many)(_ ++ _)

  def skipThen[B](other: Parser[B]): Parser[B] = (this and other)((_, b) => b)

  def thenSkip[B](other: Parser[B]): Parser[A] = (this and other)((a, _) => a)

  def ifExpect[B,T,F,R](cond:Parser[B])(thenParser:Parser[T])(_then: (A,T)=>R)(elseParser:Parser[F])(_else: (A,F) => R): Parser[R] = Parser { state =>
    run(state) match {
      case Ok((a, newState)) =>
        cond.run(newState) match {
          case Ok((b, s)) => thenParser.map(t => _then(a, t)).run(s)
          case _:Err => elseParser.map(f => _else(a, f)).run(newState) match
            case Ok((r, s)) => Ok((r, s))
            case e:Err => e
        }
      case e:Err => e
    }
  }

  override def apply(v1: State): Result[(A, State)] = run(v1)
}

//noinspection TypeAnnotation,ScalaWeakerAccess
object Parser {
  def apply[A](parser: State => Result[(A, State)]) = new Parser(parser)
  def pure[A](value: A) = Parser { state => (value, state).ok }
  def err[A](message: String) = Parser { state => Err(state.input, (message, state.index)) }
  def err[A](message: String, index: Int) = Parser { state => Err(state.input,(message, index)) }
  
  def id(description:String = "NOTHING") =  apply{
    case State(input,index) =>
      if (index < input.length) (input(index), State(input, index + 1)).ok
      else Err(input,(s"Expected $description but got end of input", index-1))
  }

  // 把 target 转义，比如换行符 \n 转为 '\n'

  def char(target: Char) = id(target.toString).flatMap{ c =>
    if (c == target) Parser.pure(c)
    else Parser.err(s"Expected '${if (target == '\n') "\\n" else target.toString}' but got '$c'")
  }

  def pred(predicate: Char => Boolean, description:String = "NOTHING") = id(description).flatMap{ c =>
    if (predicate(c)) Parser.pure(c)
    else Parser.err(s"Expected a $description but got '$c'")
  }

  def digit = pred(_.isDigit, "digit")
  def letter = pred(_.isLetter, "letter")
}



