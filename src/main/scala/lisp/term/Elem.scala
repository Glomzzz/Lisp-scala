package com.skillw
package lisp.term

import lisp.term.Applicable.Lambda
import lisp.process.Desugar

import lisp.util.Result


enum Elem {
  case Var(index:Int,name: String)
  case Num(index:Int,value: Double)
  case Str(index:Int,value: String)
  case Com(index:Int,exprs: Elem*)

  def pos: Int = this match
    case Var(index,_) => index
    case Num(index,_) => index
    case Str(index,_) => index
    case Com(index,_) => index
  def desugar: Result[Expr] = Desugar.desugar(this)

  override def toString: String = this match
    case Var(_, name) => name
    case Num(_, value) => value.toString
    case Str(_, value) => s""""$value""""
    case Com(_, exprs*) => s"(${exprs.mkString(" ")})"


  override def equals(obj: Any): Boolean = (this, obj) match
    case (Var(_,name1), Var(_,name2)) => name1 == name2
    case (Num(_,value1), Num(_,value2)) => value1 == value2
    case (Str(_,value1), Str(_,value2)) => value1 == value2
    case (Com(_, exprs1*), Com(_, exprs2*)) => exprs1 == exprs2
    case (_, _) => false
}
object Elem{
  def variable(name: String): Elem = Var(-1,name)
  def num(value: Double): Elem = Num(-1,value)
  def str(value: String): Elem = Str(-1,value)
  def com(exprs: Elem*): Elem = Com(-1,exprs: _*)
}

