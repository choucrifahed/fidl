/*
 * Copyright 2014 Choucri FAHED
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

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import java.util.Currency

trait Common {

  type TimeStep = Int
  type Date = TimeStep
  // type Date = org.joda.time.LocalDate
  type Currency = java.util.Currency

  type RandomVariable[T] = Seq[T]
  type ValueProcess[T] = Seq[RandomVariable[T]]

  val today = LocalDate.now
  val infiniteHorizon = new LocalDate(10000, 1, 1)

  val dateFormatter = DateTimeFormat.forPattern("yyyyMMdd")

  // Common currencies
  val EUR = Currency.getInstance("EUR")
  val USD = Currency.getInstance("USD")
  val GBP = Currency.getInstance("GBP")
  val CHF = Currency.getInstance("CHF")
  val JPY = Currency.getInstance("JPY")
  val CAD = Currency.getInstance("CAD")
  val AUD = Currency.getInstance("AUD")

  // FIXME Uncomment when Date switched back to LocalDate
  //  implicit def stringToDate(string: String): Date =
  //    dateFormatter.parseLocalDate(string)
}

object Common extends Common
