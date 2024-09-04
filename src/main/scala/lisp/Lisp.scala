package com.skillw
package lisp

import lisp.scope.Context
import lisp.term.Expr
import lisp.term.Expr.*

import lisp.term.Applicable.Lambda

enum LispOption {
  case ShowAtoms
  case ShowExprs
  case ShowContext
  case ShowEvals
}

class Lisp(options: LispOption*) {

  import lisp.process.LispParser.program
  import LispOption.*
  import util.*

  def option(option: LispOption)(todo: => Unit) = if options.contains(option) then todo

  private def usedVars(expr: Expr, vars: collection.mutable.Set[String] = collection.mutable.Set()): collection.mutable.Set[String] = {
    expr match
      case Var(name,_) => vars.add(name); vars
      case Com(exprs,_) => exprs.foreach(usedVars(_, vars)); vars
      case App(_, args,_) => args.foreach(usedVars(_, vars)); vars
      case Def(_, value,_) => usedVars(value, vars)
      case Abs(Lambda(params, body),_) =>
        val bodyVars = usedVars(body).filterNot(params.contains)
        vars.addAll(bodyVars)
        vars
      case _ => vars
  }

  def eval(input: String, ctx: Map[String, Expr] = Map()): Expr = {
    val atoms = program.parse(input).unwrap(input)

    option(ShowAtoms) {
      println()
      println("Atoms:")
      atoms.foreach(println)
    }

    val exprs = atoms.map(_.desugar).sequence.unwrap(input)

    option(ShowExprs) {
      println()
      println("Exprs:")
      exprs.foreach(println)
    }

    val ctx = Context()
    var result = Com()
    println()
    println("Evaluating:")
    exprs.foreach(expr => {
      result = expr.eval(ctx).unwrap(input)
      if options.contains(ShowContext) && options.contains(ShowEvals) then {
        println()
        println(s" >> $expr")
        val used = usedVars(expr)
        ctx.unduplicates.filter((name, _) => used.contains(name)).foreach {
          case (name, value) => println(s"       >      $name -> $value")
        }
        println(s" => $result")
      }
      else {
        option(ShowContext) {
          println()
          println("Context:")
          val used = usedVars(expr)
          ctx.unduplicates.filter((name, _) => used.contains(name)).foreach{
            case (name, value) => println(s"       >      $name -> $value")
          }
        }
        option(ShowEvals) {
          println()
          println(s" >> $expr")
          println(s" => $result")
        }
      }
    })
    result
  }
}