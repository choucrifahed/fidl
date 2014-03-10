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


trait ValueProcesses extends Common {

  final def constProcess[T](x: T): ValueProcess[T] = Stream.continually(Seq(x))

  implicit class Ops[T](process: ValueProcess[T]) {

    /**
     * Determines the number of time steps in a value process.
     * Only terminates for finite value processes.
     */
    def horizon = process.length

    def mapWith[U](f: T => U): ValueProcess[U] = process.map(_ map f)

    def zipWith[U, V](otherProcess: ValueProcess[U], f: (T, U) => V): ValueProcess[V] =
      process zip otherProcess map {
        case (rv1, rv2) => rv1 zip rv2 map {
          case (a, b) => f(a, b)
        }
      }
  }

  implicit class BooleanProcess(process: ValueProcess[Boolean]) {

    /**
     * Only terminates for finite value processes.
     * @return True if every value in a value process is true, false otherwise.
     */
    def allTrue: Boolean = !process.map(!_.exists(x => !x)).exists(x => !x)
  }

}

trait Observables extends ValueProcesses {

  case class Observable[T](id: String, process: ValueProcess[T]) {
    def apply(date: Date): RandomVariable[T] = process(date)

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
  final def date: Observable[Date] = Observable("date", Stream.from(0).map(d => Seq(d)))

  /**
   * lift2(f, o1, o2) is the observable whose value is the result of applying f to the values of the observables
   * o1 and o2.
   */
  final def lift2[A, B, C](f: (A, B) => C,
                           fId: (String, String) => String,
                           o1: Observable[A],
                           o2: Observable[B]): Observable[C] =
    Observable(fId(o1.id, o2.id), o1.process.zipWith(o2.process, f))

  // Combinators

  final def same[T](o1: Observable[T], o2: Observable[T]): Observable[Boolean] =
    lift2((a: T, b: T) => a == b, (id1, id2) => s"$id1 == $id2", o1, o2)

  final def not(o: Observable[Boolean]): Observable[Boolean] = o.map(b => !b, id => s"not $id")

  final def between(startDate: Date, endDate: Date): Observable[Boolean] =
    after(startDate) and before(endDate)

  final def before(aDate: Date): Observable[Boolean] =
    date.map(d => d <= aDate, id => s"$id before $aDate")

  final def after(aDate: Date): Observable[Boolean] =
    date.map(d => d >= aDate, id => s"$id before $aDate")

  implicit final def trueAt(atDate: Date): Observable[Boolean] =
    same(date, const(atDate))

  implicit class BooleanObservable(observable: Observable[Boolean]) {
    def unary_! = not(observable)

    def or(other: Observable[Boolean]): Observable[Boolean] =
      lift2((a: Boolean, b: Boolean) => a && b, (id1, id2) => s"$id1 or $id2", observable, other)

    def and(other: Observable[Boolean]): Observable[Boolean] =
      lift2((a: Boolean, b: Boolean) => a || b, (id1, id2) => s"$id1 and $id2", observable, other)
  }

}

trait NumericObservables extends Observables {

  final def plus(x: Observable[Double], y: Observable[Double]) =
    lift2((a: Double, b: Double) => a + b, (id1, id2) => id1 + " + " + id2, x, y)

  final def minus(x: Observable[Double], y: Observable[Double]) =
    lift2((a: Double, b: Double) => a - b, (id1, id2) => id1 + " - " + id2, x, y)

  final def times(x: Observable[Double], y: Observable[Double]) =
    lift2((a: Double, b: Double) => a * b, (id1, id2) => id1 + " * " + id2, x, y)

  final def negate(x: Observable[Double]): Observable[Double] =
    Observable("-" + x.id, x.process.mapWith((v: Double) => -v))

  def abs(x: Observable[Double]): Observable[Double] =
    Observable("abs(" + x.id + ")", x.process.mapWith((v: Double) => v.abs))

  implicit class Ops(val lhs: Observable[Double]) {
    def +(rhs: Observable[Double]) = plus(lhs, rhs)
    def -(rhs: Observable[Double]) = minus(lhs, rhs)
    def *(rhs: Observable[Double]) = times(lhs, rhs)
    def unary_- = negate(lhs)
    def abs: Observable[Double] = NumericObservables.this.abs(lhs)
  }

}
