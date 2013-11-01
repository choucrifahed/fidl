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

trait ObservableIsNumeric extends Numeric[Observable] {

  def plus(x: Observable, y: Observable) =
    d => x(d) + y(d)

  def minus(x: Observable, y: Observable) =
    d => x(d) - y(d)

  def times(x: Observable, y: Observable) =
    d => x(d) * y(d)

  def negate(x: Observable): Observable =
    d => -x(d)

  def const(x: Double): Observable =
    d => x

  def fromInt(x: Int): Observable =
    const(x)

  def toInt(x: Observable): Int =
    x(DateTime.now()).toInt

  def toLong(x: Observable): Long =
    x(DateTime.now()).toLong

  def toFloat(x: Observable): Float =
    x(DateTime.now()).toFloat

  def toDouble(x: Observable): Double =
    x(DateTime.now())

  def compare(x: Observable, y: Observable): Int = {
    val now = DateTime.now()
    x(now) compare y(now)
  }

}
