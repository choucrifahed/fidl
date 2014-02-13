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
import java.util.Currency
import Implicits._

sealed trait Contract {

  def start: DateTime

  def expiry: DateTime

  def isZero: Boolean = false

  final def and(other: Contract) =
    if (this.isZero || this.hasExpired) other
    else if (other.isZero || other.hasExpired) this
    else And(this, other)

  final def or(other: Contract) =
    if (this.isZero || this.hasExpired) other
    else if (other.isZero || other.hasExpired) this
    else Or(this, other)

  final def hasExpired: Boolean =
    expiry < Today
}

trait ContractPrimitive {

  def Zero(currency: Currency): ElementaryContract =
    ElementaryContract(Implicits.zero, currency)

  def One(currency: Currency): ElementaryContract =
    ElementaryContract(Implicits.one, currency)

  def anytime(contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.anytime
      case And(left, right) => And(anytime(left), anytime(right))
      case Or(left, right) => Or(anytime(left), anytime(right))
    }

  def get(contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.get
      case And(left, right) => And(get(left), get(right))
      case Or(left, right) => Or(get(left), get(right))
    }

  def give(contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.give
      case And(left, right) => And(give(left), give(right))
      case Or(left, right) => Or(give(left), give(right))
    }

  def truncate(date: DateTime, contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.truncate(date)
      case And(left, right) => And(truncate(date, left), truncate(date, right))
      case Or(left, right) => Or(truncate(date, left), truncate(date, right))
    }

  def scale(observable: Observable[Double], contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.scale(observable)
      case And(left, right) => And(scale(observable, left), scale(observable, right))
      case Or(left, right) => Or(scale(observable, left), scale(observable, right))
    }

  def scale(constant: Double, contract: Contract): Contract =
    scale(const(constant), contract)
}

case class And(left: Contract, right: Contract) extends Contract {

  override val start =
    if (left.start < right.start) left.start else right.start

  override val expiry =
    if (left.expiry < right.expiry) right.expiry else left.expiry

  override def toString =
    s"($left) and ($right)"

}

case class Or(left: Contract, right: Contract) extends Contract {

  override val start =
    if (left.start < right.start) left.start else right.start

  override val expiry =
    if (left.expiry < right.expiry) right.expiry else left.expiry

  override def toString =
    s"($left) or ($right)"

}

case class ElementaryContract(observable: Observable[Double],
                              currency: Currency,
                              start: DateTime = Today,
                              expiry: DateTime = InfiniteHorizon,
                              side: Side = Buy) extends Contract {

  override def isZero: Boolean =
    observable.id == "0.0" || observable.id == "-0.0"

  def scale(observable: Observable[Double]): ElementaryContract =
    if (isZero) this
    else if (this.observable.id == "1.0") copy(observable = observable)
    else copy(observable = this.observable * observable)

  def scale(constant: Double): ElementaryContract =
    scale(const(constant))

  def give: ElementaryContract =
    if (isZero) this
    else copy(side = -side)

  def get: ElementaryContract =
    if (isZero || expiry == InfiniteHorizon) this
    else copy(start = expiry)

  def anytime: ElementaryContract =
    copy(start = Today)

  def truncate(date: DateTime): ElementaryContract =
    if (isZero) this
    else copy(expiry = date)

  override def toString = {
    val action = if (side == Buy) "receive" else "pay"
    val startStr = if (start == Today) "today" else "at " + start.toString(DateFormatter)
    val expiryStr = if (expiry == InfiniteHorizon) "" else "until " + expiry.toString(DateFormatter)

    s"$action $observable $currency $startStr$expiryStr"
  }

}
