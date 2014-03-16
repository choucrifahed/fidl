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

import java.util.Currency

trait Common {

  type TimeStep = Int
  type Date = TimeStep
  type Currency = java.util.Currency

  // Common currencies
  val AUD = Currency.getInstance("AUD")
  val CAD = Currency.getInstance("CAD")
  val CHF = Currency.getInstance("CHF")
  val EUR = Currency.getInstance("EUR")
  val GBP = Currency.getInstance("GBP")
  val JPY = Currency.getInstance("JPY")
  val USD = Currency.getInstance("USD")

  // FIXME Uncomment when Date switched back to LocalDate
  //  type Date = org.joda.time.LocalDate
  //
  //  val today = LocalDate.now
  //  val infiniteHorizon = new LocalDate(10000, 1, 1)
  //
  //  val dateFormatter = DateTimeFormat.forPattern("yyyyMMdd")
  //
  //  implicit def stringToDate(string: String): Date =
  //    dateFormatter.parseLocalDate(string)
}
