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

trait Contract {

  def start: DateTime
  def expiry: DateTime

  def isZero: Boolean = false

  def and(other: Contract) =
    if (this.isZero) other
    else if (other.isZero) this
    else And(this, other)

}

trait ContractCombinator {

  def Zero(currency: Currency): BaseContract =
    BaseContract(Implicits.zero, currency)

  def One(currency: Currency): BaseContract =
    BaseContract(Implicits.one, currency)

  def get(contract: Contract): Contract =
    contract match {
      case base: BaseContract => base.get
      case And(left, right) => And(get(left), get(right))
    }

  def give(contract: Contract): Contract =
    contract match {
      case base: BaseContract => base.give
      case And(left, right) => And(give(left), give(right))
    }

  def truncate(date: DateTime, contract: Contract): Contract =
    contract match {
      case base: BaseContract => base.truncate(date)
      case And(left, right) => And(truncate(date, left), truncate(date, right))
    }

}

case class And(left: Contract, right: Contract) extends Contract {

  override val start =
    if (left.start < right.start) left.start else right.start

  override val expiry =
    if (left.expiry < right.expiry) right.expiry else left.expiry

  override def toString =
    s"($left) and ($right)"

}

case class BaseContract(underlying: Observable,
                        currency: Currency,
                        start: DateTime = Today,
                        expiry: DateTime = InfiniteHorizon,
                        side: Side = Buy) extends Contract {

  override def isZero: Boolean =
    underlying.desc == "0.0" || underlying.desc == "-0.0"

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
    val startStr = if (start == Today) "now" else "at " + start.toString(DateFormatter)
    val expiryStr = if (expiry == InfiniteHorizon) "" else "until " + expiry.toString(DateFormatter)

    s"$action $underlying $currency $startStr$expiryStr"
  }

}
