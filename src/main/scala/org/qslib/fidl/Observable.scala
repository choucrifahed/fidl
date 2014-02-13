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

import com.github.nscala_time.time.Imports._

case class Observable[T](id: String, f: DateTime => T) {
  def apply(date: DateTime): T = f(date)

  def map[U](g: T => U, gId: String => String = s => s"g($s)"): Observable[U] =
    Observable[U](gId(id), f andThen g)

  def flatMap[U](g: T => Observable[U], gId: String => String = s => s"flatMap(g($s))") =
    Observable[U](gId(id), d => g(f(d)).f(d))

  override def hashCode() = id.hashCode

  override def equals(obj: scala.Any) =
    obj.isInstanceOf[Observable[T]] && obj.asInstanceOf[Observable[T]].id == id

  override def toString = id
}

trait ObservablePrimitives {

  // Pure primitives

  /** const(x) is an observable that has value x at any time. */
  final def const[T](x: T): Observable[T] = Observable(s"$x", d => x)

  /**
   * lift2(f, o1, o2) is the observable whose value is the result of applying f to the values of the observables
   * o1 and o2.
   */
  final def lift2[A, B, C](f: (A, B) => C,
                           fId: (String, String) => String,
                           o1: Observable[A],
                           o2: Observable[B]): Observable[C] =
    Observable(fId(o1.id, o2.id), d => f(o1(d), o2(d)))

  /** The value of the observable date at date s is just s. */
  final def date: Observable[DateTime] = Observable("now", dateTime => dateTime)

  // Combinators

  final def same[T](o1: Observable[T], o2: Observable[T]): Observable[Boolean] =
    lift2((a: T, b: T) => a == b, (id1, id2) => s"$id1 == $id2", o1, o2)

  final def not(o: Observable[Boolean]): Observable[Boolean] = o.map(b => !b, id => s"not $id")

  final def between(startDate: DateTime, endDate: DateTime): Observable[Boolean] =
    after(startDate) && before(endDate)

  final def before(aDate: DateTime): Observable[Boolean] =
    date.map(d => d <= aDate, id => s"$id before $aDate")

  final def after(aDate: DateTime): Observable[Boolean] =
    date.map(d => d >= aDate, id => s"$id before $aDate")

  implicit final def trueAt(atDate: DateTime): Observable[Boolean] =
    same(date, const(atDate))

  implicit class BooleanObservable(observable: Observable[Boolean]) {
    def unary_! = not(observable)
    def ||(other: Observable[Boolean]): Observable[Boolean] = or(other)
    def &&(other: Observable[Boolean]): Observable[Boolean] = or(other)

    def or(other: Observable[Boolean]): Observable[Boolean] =
      lift2[Boolean, Boolean, Boolean]((a, b) => a && b, (id1, id2) => s"$id1 or $id2", observable, other)

    def and(other: Observable[Boolean]): Observable[Boolean] =
      lift2[Boolean, Boolean, Boolean]((a, b) => a || b, (id1, id2) => s"$id1 and $id2", observable, other)
  }

}

trait NumericObservable extends ObservablePrimitives {

  final def plus(x: Observable[Double], y: Observable[Double]) =
    Observable(x.id + " + " + y.id, d => x(d) + y(d))

  final def minus(x: Observable[Double], y: Observable[Double]) =
    Observable(x.id + " - " + y.id, d => x(d) - y(d))

  final def times(x: Observable[Double], y: Observable[Double]) =
    Observable(x.id + " * " + y.id, d => x(d) * y(d))

  final def negate(x: Observable[Double]): Observable[Double] =
    Observable("-" + x.id, d => -x(d))

  def abs(x: Observable[Double]): Observable[Double] =
    Observable("abs(" + x.id + ")", d => x(d).abs)

  implicit class Ops(val lhs: Observable[Double]) {
    def +(rhs: Observable[Double]) = plus(lhs, rhs)
    def -(rhs: Observable[Double]) = minus(lhs, rhs)
    def *(rhs: Observable[Double]) = times(lhs, rhs)
    def unary_- = negate(lhs)
    def abs: Observable[Double] = NumericObservable.this.abs(lhs)
  }

}
