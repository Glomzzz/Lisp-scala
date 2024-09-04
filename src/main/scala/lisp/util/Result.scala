package com.skillw
package lisp.util


extension[T] (list:Seq[T])
  def mapResult[R](f: T => Result[R]): Result[List[R]] = {
    var results = List[R]()
    var err: Option[Err] = None
    list.foreach(e => f(e) match {
      case Ok(value) => results = results :+ value
      case e: Err => err = Some(err.getOrElse(Err("")) ++ e)
    })
    err match {
      case Some(e) => e
      case None => results.ok
    }
  }
  
extension[T] (result:Result[Result[T]])
  def flatten:Result[T] = result match {
    case Ok(value) => value
    case e:Err => e
  }  
extension[A,B] (tuple:(Result[A],Result[B]))
  def map[R](f:(A,B) => R): Result[R] = tuple match {
    case (Ok(a), Ok(b)) => f(a,b).ok
    case (e:Err, Ok(_)) => e
    case (Ok(_), e:Err) => e
    case (e1:Err, e2:Err) => e1 ++ e2
  }
extension[T] (list:Seq[Result[T]])
  def sequence:Result[Seq[T]] = {
    var results = List[T]()
    var err: Option[Err] = None
    list.foreach {
      case Ok(value) => results = results :+ value
      case e: Err => err = Some(err.getOrElse(Err("")) ++ e)
    }
    err match {
      case Some(e) => e
      case None => results.ok
    }
  }

sealed trait Result[+T]{
  def unwrap(input:String = "") = this match {
    case Ok(value) => value
    case e:Err => if(input.isEmpty) e.error else e.input = input; e.error
  }

  def success = this match {
    case Ok(_) => true
    case e:Err => false
  }

  def map[B](f: T => B): Result[B] = this match {
    case Ok(value) => f(value).ok
    case e:Err => e
  }

  def mapResult[B] (f: T => Result[B]): Result[B] = this match {
    case Ok(value) => f(value)
    case e:Err => e
  }

  def withError(e:Err) = this match {
    case Ok(_) => e
    case e1:Err => e1 ++ e
  }
}

extension[T] (any:T)
  def ok = Ok(any)

case class Ok[T](value: T) extends Result[T]
case class Err(var input:String, errors:(String,Int)*) extends Result[Nothing]{

  def ++(err:Err) = {
    var newErrors = List(errors: _*)
    err.errors.filterNot(e1 => newErrors.contains(e1)).foreach({ e =>
      newErrors = newErrors :+ e
    })
    Err(input, newErrors: _*)
  }

  def errMessage(message:String,index:Int) = {
    val before = input.take(index)
    val line = before.count(_ == '\n') + 1
    val column = before.reverse.takeWhile(_ != '\n').length - 1
    val lines = input.split('\n')
    s"""
       |  Error occurred : $message at line $line , $column:
       |
       |      ${if (line > 1) lines(line - 2) else ""}
       |      ${lines(line - 1)}
       |      ${" " * column}^
       |      ${if (line < lines.length) lines(line) else ""}
       |
    """.stripMargin.filterNot(_ == '\r')
  }

  def error = throw new Exception(errors.map((err,index) => errMessage(err,index)).mkString("\n\n"))
}
object Err{
  def of(message:String, index:Int) = Err("", (message,index))
}