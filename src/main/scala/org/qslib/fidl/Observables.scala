/*
 * Copyright 2013 Choucri FAHED
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.qslib.fidl

trait Observables extends ValueProcesses {

  case class Observable[T](id: String, process: ValueProcess[T]) {
    // FIXME when real dates are used
    def apply(date: Date): ValueProcess[T] = process

    def map[U](f: T => U, gId: String => String = s => s"g($s)"): Observable[U] =
      Observable[U](gId(id), process mapWith f)

    override def hashCode() = id.hashCode

    override def equals(obj: scala.Any) =
      obj.isInstanceOf[Observable[T]] && obj.asInstanceOf[Observable[T]].id == id

    override def toString = id
  }

  // Pure primitives

  /** const(x) is an observable that has value x at any time. */
  final def const[T](x: T): Observable[T] = Observable(s"$x", constProcess(x))

  /** The value of the observable date at date s is just s. */
  val date: Observable[Date] = Observable("date", dateProcess)

  /**
   * lift2(o1, o2)(f) is the observable whose value is the result of applying f to the values of the observables
   * o1 and o2. Currying is used to enable type inference.
   */
  final def lift2[A, B, C](o1: Observable[A],
                           o2: Observable[B],
                           fId: (String, String) => String)
                          (f: (A, B) => C): Observable[C] =
    Observable(fId(o1.id, o2.id), o1.process.zipWith(o2.process)(f))

  // Combinators

  final def same[T](o1: Observable[T], o2: Observable[T]): Observable[Boolean] = lift2(o1, o2, _ + " == " + _)(_ == _)

  final def not(o: Observable[Boolean]): Observable[Boolean] = o.map(b => !b, id => s"not $id")

  final def between(startDate: Date, endDate: Date): Observable[Boolean] = after(startDate) and before(endDate)

  final def before(aDate: Date): Observable[Boolean] = date.map(d => d <= aDate, id => s"$id before $aDate")

  final def after(aDate: Date): Observable[Boolean] = date.map(d => d >= aDate, id => s"$id before $aDate")

  implicit final def trueAt(atDate: Date): Observable[Boolean] = same(date, const(atDate))

  implicit final class BooleanObservable(observable: Observable[Boolean]) {
    def unary_! = not(observable)

    def or(other: Observable[Boolean]): Observable[Boolean] = lift2(observable, other, _ + " or " + _)(_ || _)
    def and(other: Observable[Boolean]): Observable[Boolean] = lift2(observable, other, _ + " and " + _)(_ && _)
  }

}

trait NumericObservables extends Observables {

  final def plus(x: Observable[Double], y: Observable[Double]): Observable[Double] = lift2(x, y, _ + " + " + _)(_ + _)
  final def minus(x: Observable[Double], y: Observable[Double]): Observable[Double] = lift2(x, y, _ + " - " + _)(_ - _)
  final def times(x: Observable[Double], y: Observable[Double]): Observable[Double] = lift2(x, y, _ + " * " + _)(_ * _)

  final def negate(x: Observable[Double]): Observable[Double] =
    Observable("-" + x.id, x.process mapWith {
      v => -v
    })

  final def abs(x: Observable[Double]): Observable[Double] =
    Observable("abs(" + x.id + ")", x.process mapWith {
      v => v.abs
    })

  implicit final class Ops(val lhs: Observable[Double]) {
    def +(rhs: Observable[Double]): Observable[Double] = plus(lhs, rhs)
    def -(rhs: Observable[Double]): Observable[Double] = minus(lhs, rhs)
    def *(rhs: Observable[Double]): Observable[Double] = times(lhs, rhs)

    def unary_- : Observable[Double] = negate(lhs)
    def abs: Observable[Double] = NumericObservables.this.abs(lhs)

    // Relational operators
    def <(rhs: Observable[Double]): Observable[Boolean] = lift2(lhs, rhs, _ + " < " + _)(_ < _)
    def <=(rhs: Observable[Double]): Observable[Boolean] = lift2(lhs, rhs, _ + " <= " + _)(_ <= _)
    def ===(rhs: Observable[Double]): Observable[Boolean] = same(lhs, rhs)
    def >=(rhs: Observable[Double]): Observable[Boolean] = lift2(lhs, rhs, _ + " >= " + _)(_ >= _)
    def >(rhs: Observable[Double]): Observable[Boolean] = lift2(lhs, rhs, _ + " > " + _)(_ > _)
  }

}
