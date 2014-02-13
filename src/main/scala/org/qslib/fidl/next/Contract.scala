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

package org.qslib.fidl.next

import java.util.Currency
import org.qslib.fidl.{NumericObservable, Observable}
import org.joda.time.DateTime


trait Contract

trait ContractPrimitives {

  /** Zero is a contract that has no rights and no obligations. */
  def zeroC: Contract

  /** If you acquire one(k), you immediately receive one unit of the currency k. */
  def one(currency: Currency): Contract

  /** To acquire give(c) is to acquire all of c's rights as obligations, and vice versa. */
  def give(contract: Contract): Contract

  /** If you acquire c1 and c2, you immediately acquire both c1 and c2. */
  def and(contract1: Contract, contract2: Contract): Contract

  /** If you acquire c1 or c2 you must immediately acquire your choice of either c1 or c2 (but not both). */
  def or(contract1: Contract, contract2: Contract): Contract

  /**
   * If you acquire cond(b, c1, c2), you acquire c1 if the observable b is true at the moment of acquisition, and
   * c2 otherwise.
   */
  def cond(observable: Observable[Boolean], contract1: Contract, contract2: Contract): Contract

  /**
   * If you acquire scale(o, c), then you acquire c at the same moment, except that all the payments of c are
   * multiplied by the value of the observable o at the moment of acquisition.
   */
  def scale(observable: Observable[Double], contract: Contract): Contract

  /**
   * If you acquire when(o, c), you must acquire c as soon as observable o subsequently becomes true.
   * It is therefore worthless in states where o will never again be true.
   */
  def when(observable: Observable[Boolean], contract: Contract): Contract

  /**
   * Once you acquire anytime(o, c), you may acquire at any time the observable o is true. The compound contract is
   * therefore worthless in states where o will never again be true.
   */
  def anytime(observable: Observable[Boolean], contract: Contract): Contract

  /**
   * Once acquired, until (o, c) is exactly like c except that it must be abandoned when observable o becomes true.
   * In states in which o is true, the compound contract is therefore worthless, because it must be abandoned
   * immediately.
   */
  def until(observable: Observable[Boolean], contract: Contract): Contract

  /** Syntactic sugar */
  implicit class ContractOps(val contract: Contract) {
    def unary_- = give(contract)
    def &&(other: Contract) = and(other)
    def ||(other: Contract) = or(other)
    def *(observable: Observable[Double]) = scale(observable)
    def at(observable: Observable[Boolean]) = when(observable)

    def and(other: Contract) = ContractPrimitives.this.and(contract, other)
    def or(other: Contract) = ContractPrimitives.this.or(contract, other)
    def scale(observable: Observable[Double]) = ContractPrimitives.this.scale(observable, contract)
    def when(observable: Observable[Boolean]) = ContractPrimitives.this.when(observable, contract)
    def anytime(observable: Observable[Boolean]) = ContractPrimitives.this.anytime(observable, contract)
    def until(observable: Observable[Boolean]) = ContractPrimitives.this.until(observable, contract)

    def onlyIf(observable: Observable[Boolean]) = new {
      def otherwise(other: Contract) = cond(observable, contract, other)
    }
  }

}

trait CommonContracts extends ContractPrimitives with NumericObservable {

  /** Immediately receive an amount of cash in a given currency. */
  final def cash(amount: Double, currency: Currency): Contract = one(currency) * const(amount)

  /** Zero Coupon Bond: receive an amount of cash in a given currency at a payment date. */
  final def zcb(amount: Double, currency: Currency, date: DateTime): Contract = cash(amount, currency) at date

  /** A European option gives the right to acquire an underlying contract only at expiry. */
  final def european(underlying: Contract, expiry: DateTime): Contract = (underlying or zeroC) at expiry

  /** An American option gives the right to acquire an underlying contract anytime between two dates. */
  final def american(underlying: Contract, start: DateTime, expiry: DateTime): Contract =
    (underlying or zeroC) anytime between(start, expiry)

  /** A knock-in barrier option gives the right to acquire the underlying contract only when the barrier is broken. */
  final def knockIn(underlying: Contract, start: DateTime, expiry: DateTime, barrier: Observable[Boolean]): Contract =
    american(underlying, start, expiry) when barrier

  /** A knock-out barrier option gives the right to acquire the underlying contract until the barrier is broken. */
  final def knockOut(underlying: Contract, start: DateTime, expiry: DateTime, barrier: Observable[Boolean]): Contract =
    american(underlying, start, expiry) until barrier
}
