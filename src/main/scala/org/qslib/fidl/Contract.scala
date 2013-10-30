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

  // If expiry is None, it means that the contract never expires and can be acquired anytime.
  def expiry: DateTime

  def give: Contract

  def and(other: Contract): Contract = And(this, other)

  final def unary_- : Contract = give
  final def andGive(other: Contract): Contract = and(other.give)
}

object Contract {
  def give(c: Contract): Contract = c.give
}

case class And(left: Contract, right: Contract) extends Contract {
  override val start = if (left.start < right.start) left.start else right.start
  override val expiry = if (left.expiry < right.expiry) right.expiry else left.expiry
  override def toString = left + " and " + right
  override def give: Contract = And(left.give, right.give)
}

case class BaseContract(obs: Observable,
                        currency: Currency,
                        start: DateTime = Now,
                        expiry: DateTime = InfiniteHorizon,
                        side: Side = Buy) extends Contract {

  def give = this.copy(side = -side)
}

case object Zero extends Contract {
  override val start = Now
  override val expiry = InfiniteHorizon
  override def and(other: Contract): Contract = other
  override def give: Contract = this
}

object One {
  def apply(currency: Currency) = BaseContract(one, currency)
}
