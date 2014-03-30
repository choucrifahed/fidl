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

trait ContractTerms extends Observables {

  sealed trait Contract {

    // Syntactic sugar methods for infix use

    @inline final def unary_- = give(this)
    @inline final def &&(other: Contract) = and(other)
    @inline final def ||(other: Contract) = or(other)
    @inline final def *(observable: Observable[Double]) = scale(observable)
    @inline final def at(observable: Observable[Boolean]) = when(observable)

    @inline final def and(other: Contract) = ContractTerms.this.and(this, other)
    @inline final def or(other: Contract) = ContractTerms.this.or(this, other)
    @inline final def scale(observable: Observable[Double]) = ContractTerms.this.scale(observable, this)
    @inline final def when(observable: Observable[Boolean]) = ContractTerms.this.when(observable, this)
    @inline final def anytime(observable: Observable[Boolean]) = ContractTerms.this.anytime(observable, this)
    @inline final def until(observable: Observable[Boolean]) = ContractTerms.this.until(observable, this)

    @inline final def onlyIf(observable: Observable[Boolean]) = new {
      def otherwise(other: Contract) = cond(observable, Contract.this, other)
    }
  }

  /** Zero is a contract that has no rights and no obligations. */
  case object zero extends Contract

  /** If you acquire one(k), you immediately receive one unit of the currency k. */
  case class one(currency: Currency) extends Contract

  /** To acquire give(c) is to acquire all of c's rights as obligations, and vice versa. */
  case class give(contract: Contract) extends Contract

  /** If you acquire c1 and c2, you immediately acquire both c1 and c2. */
  case class and(contract1: Contract, contract2: Contract) extends Contract

  /** If you acquire c1 or c2 you must immediately acquire your choice of either c1 or c2 (but not both). */
  case class or(contract1: Contract, contract2: Contract) extends Contract

  /**
   * If you acquire cond(b, c1, c2), you acquire c1 if the observable b is true at the moment of acquisition, and
   * c2 otherwise.
   */
  case class cond(observable: Observable[Boolean], contract1: Contract, contract2: Contract) extends Contract

  /**
   * If you acquire scale(o, c), then you acquire c at the same moment, except that all the payments of c are
   * multiplied by the value of the observable o at the moment of acquisition.
   */
  case class scale(observable: Observable[Double], contract: Contract) extends Contract

  /**
   * If you acquire when(o, c), you must acquire c as soon as observable o subsequently becomes true.
   * It is therefore worthless in states where o will never again be true.
   */
  case class when(observable: Observable[Boolean], contract: Contract) extends Contract

  /**
   * Once you acquire anytime(o, c), you may acquire at any time the observable o is true. The compound contract is
   * therefore worthless in states where o will never again be true.
   */
  case class anytime(observable: Observable[Boolean], contract: Contract) extends Contract

  /**
   * Once acquired, until (o, c) is exactly like c except that it must be abandoned when observable o becomes true.
   * In states in which o is true, the compound contract is therefore worthless, because it must be abandoned
   * immediately.
   */
  case class until(observable: Observable[Boolean], contract: Contract) extends Contract

}

trait CommonContracts extends ContractTerms with NumericObservables {

  /** Immediately receive an amount of cash in a given currency. */
  final def cash(amount: Double, currency: Currency): Contract = one(currency) * const(amount)

  /** Zero Coupon Bond: receive an amount of cash in a given currency at a payment date. */
  final def zcb(amount: Double, currency: Currency, date: Date): Contract = cash(amount, currency) at date

  /** A European option gives the right to acquire an underlying contract only at expiry. */
  final def european(underlying: Contract, expiry: Date): Contract = (underlying or zero) at expiry

  /** An American option gives the right to acquire an underlying contract anytime between two dates. */
  final def american(underlying: Contract, start: Date, expiry: Date): Contract =
    (underlying or zero) anytime between(start, expiry)

  /** A knock-in barrier option gives the right to acquire the underlying contract only when the barrier is broken. */
  final def knockIn(underlying: Contract, start: Date, expiry: Date, barrier: Observable[Boolean]): Contract =
    american(underlying, start, expiry) when barrier

  /** A knock-out barrier option gives the right to acquire the underlying contract until the barrier is broken. */
  final def knockOut(underlying: Contract, start: Date, expiry: Date, barrier: Observable[Boolean]): Contract =
    american(underlying, start, expiry) until barrier

  implicit final class DoubleToCash(amount: Double) {
    // TODO think of a more elegant syntax, maybe use macros
    def *(currency: Currency) = cash(amount, currency)
  }

}
