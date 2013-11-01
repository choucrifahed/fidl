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

  def give: Contract
  def get: Contract
  def truncate(date: DateTime): Contract

  def and(other: Contract): Contract =
    And(this, other)

  final def andGive(other: Contract): Contract =
    and(other.give)

}

trait ContractOps {

  def give(contract: Contract): Contract =
    contract.give

  def get(contract: Contract): Contract =
    contract.get

  def truncate(contract: Contract) = new {
    def at(date: DateTime) =
      contract.truncate(date)
  }

}

case class And(left: Contract, right: Contract) extends Contract {

  override val start =
    if (left.start < right.start) left.start else right.start

  override val expiry =
    if (left.expiry < right.expiry) right.expiry else left.expiry

  override def give: Contract =
    And(left.give, right.give)

  override def get: Contract =
    And(left.get, right.get)

  override def truncate(date: DateTime) =
    And(left.truncate(date), right.truncate(date))

  override def toString =
    left + " and " + right

}

case class BaseContract(obs: Observable,
                        currency: Currency,
                        start: DateTime = Now,
                        expiry: DateTime = InfiniteHorizon,
                        side: Side = Buy) extends Contract {

  override def give =
    copy(side = -side)

  override def get =
    if (expiry == InfiniteHorizon) this
    else copy(start = expiry)

  override def truncate(date: DateTime) =
    if (expiry == InfiniteHorizon) this
    else copy(expiry = date)

}

object Zero {

  def apply(currency: Currency) =
    BaseContract(zero, currency)

}

object One {

  def apply(currency: Currency) =
    BaseContract(one, currency)

}
