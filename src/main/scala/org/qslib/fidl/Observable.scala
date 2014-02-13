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

import org.joda.time.DateTime

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
}

trait NumericObservable extends ObservablePrimitives with Numeric[Observable[Double]] {

  override final def plus(x: Observable[Double], y: Observable[Double]) =
    Observable(x.id + " + " + y.id, d => x(d) + y(d))

  override final def minus(x: Observable[Double], y: Observable[Double]) =
    Observable(x.id + " - " + y.id, d => x(d) - y(d))

  override final def times(x: Observable[Double], y: Observable[Double]) =
    Observable(x.id + " * " + y.id, d => x(d) * y(d))

  override final def negate(x: Observable[Double]): Observable[Double] =
    Observable("-" + x.id, d => -x(d))

  override final def fromInt(x: Int): Observable[Double] =
    const(x)

  override final def toInt(x: Observable[Double]): Int =
    x(DateTime.now()).toInt

  override final def toLong(x: Observable[Double]): Long =
    x(DateTime.now()).toLong

  override final def toFloat(x: Observable[Double]): Float =
    x(DateTime.now()).toFloat

  override final def toDouble(x: Observable[Double]): Double =
    x(DateTime.now())

  override final def compare(x: Observable[Double], y: Observable[Double]): Int = {
    val now = DateTime.now()
    x(now) compare y(now)
  }

}
