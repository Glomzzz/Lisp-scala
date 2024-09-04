package com.skillw
package lisp.scope

import lisp.term.Expr
import lisp.term.Expr.*

import lisp.util.*


case class Context(var vars: LinkedVar = LinkedVar.Nil,builtIn:BuiltInContext = DefaultContext) {

  def get(v:Var): Result[Expr] =
    vars.find(v.name) match
        case Some((name, value)) =>
          value.eval(this).map( result=>
            add(name, result)
            result
          )
        case None => builtIn.builtIns.find(_._1 == v.name).map(_._2) match
          case Some(value) => value.ok
          case None => Err.of(s"Undefined variable: ${v.name}",v.index)
          
  def add(name: String, value: Expr): Context = {
    vars = (name, value) :: vars
    this
  }

  def copy(): Context = Context(vars)

  def filter(predicate: ((String, Expr)) => Boolean): Context = Context(vars.filter(predicate))

  def foreach(todo: ((String, Expr)) => Unit): Unit = vars.foreach(todo)

  def unduplicates: Context = Context(vars.unduplicates)

  override def toString: String = vars.toString
}

enum LinkedVar {
  case Con(name: String, value: Expr, next: LinkedVar)
  case Nil


  def filter(predicate: ((String, Expr)) => Boolean): LinkedVar = this match
    case Con(name, value, next) if predicate((name, value)) => Con(name, value, next.filter(predicate))
    case Con(_, _, next) => next.filter(predicate)
    case Nil => Nil

  def foreach(todo: ((String, Expr)) => Unit): Unit = this match
    case Con(name, value, next) =>
      todo((name, value))
      next.foreach(todo)
    case Nil => ()

  def find(name: String): Option[(String, Expr)] = this match
    case Con(n, v, next) if n == name => Some(n, v)
    case Con(_, _, next) => next.find(name)
    case Nil => None

  def unduplicates: LinkedVar = this match
    case Con(name, value, next) =>
      val rest = next.unduplicates
      if rest.find(name).isDefined then rest else Con(name, value, rest)
    case Nil => Nil

  def ::(name: String, value: Expr): LinkedVar = Con(name, value, this)

  override def toString: String = {
    this match
      case Con(name, value, next) => s"$name -> $value\n$next"
      case Nil => ""
  }
}