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

trait Contract {

  def start: DateTime
  def expiry: DateTime

  def isZero: Boolean = false

  def and(other: Contract) =
    if (this.isZero) other
    else if (other.isZero) this
    else And(this, other)

}

trait ContractTree extends Contract {

  def left: Contract
  def right: Contract

}

trait ContractPrimitive {

  def Zero(currency: Currency): ElementaryContract =
    ElementaryContract(Implicits.zero, currency)

  def One(currency: Currency): ElementaryContract =
    ElementaryContract(Implicits.one, currency)

  def get(contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.get
      case And(left, right) => And(get(left), get(right))
    }

  def give(contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.give
      case And(left, right) => And(give(left), give(right))
    }

  def truncate(date: DateTime, contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.truncate(date)
      case And(left, right) => And(truncate(date, left), truncate(date, right))
    }

  def scale(observable: Observable, contract: Contract): Contract =
    contract match {
      case contract: ElementaryContract => contract.scale(observable)
      case And(left, right) => And(scale(observable, left), scale(observable, right))
    }

  def scale(constant: Double, contract: Contract): Contract =
    scale(const(constant), contract)
}

case class And(left: Contract, right: Contract) extends ContractTree {

  override val start =
    if (left.start < right.start) left.start else right.start

  override val expiry =
    if (left.expiry < right.expiry) right.expiry else left.expiry

  override def toString =
    s"($left) and ($right)"

}

case class ElementaryContract(observable: Observable,
                              currency: Currency,
                              start: DateTime = Today,
                              expiry: DateTime = InfiniteHorizon,
                              side: Side = Buy) extends Contract {

  override def isZero: Boolean =
    observable.desc == "0.0" || observable.desc == "-0.0"

  def scale(observable: Observable): Contract =
    if (isZero) this
    else if (this.observable.desc == "1.0") copy(observable = observable)
    else copy(observable = this.observable * observable)

  def scale(constant: Double): Contract =
    scale(const(constant))

  def give =
    if (isZero) this
    else copy(side = -side)

  def get =
    if (isZero || expiry == InfiniteHorizon) this
    else copy(start = expiry)

  def truncate(date: DateTime) =
    if (isZero) this
    else copy(expiry = date)

  override def toString = {
    val action = if (side == Buy) "receive" else "pay"
    val startStr = if (start == Today) "today" else "at " + start.toString(DateFormatter)
    val expiryStr = if (expiry == InfiniteHorizon) "" else "until " + expiry.toString(DateFormatter)

    s"$action $observable $currency $startStr$expiryStr"
  }

}
