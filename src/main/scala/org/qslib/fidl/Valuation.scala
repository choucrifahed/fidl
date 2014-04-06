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

trait Valuation extends ContractTerms with Models {

  final def using(model: Model) = new {
    def in(currency: Currency) = new {
      def eval(contract: Contract): ValueProcess[Double] = contract match {
        case `zero` => constProcess(0.0)
        case one(currency2) => model.exchange(currency, currency2)
        case give(c) => -eval(c)
        case scale(o, c) => o(0) * eval(c)
        case and(c1, c2) => eval(c1) + eval(c2)
        case or(c1, c2) => eval(c1) maximum eval(c2)
        case cond(o, c1, c2) => o(0).condProcess(eval(c1), eval(c2))
        case when(o, c) => model.discount(currency)(o(0), eval(c))
        //     case anytime(o, c)  => snell  (k, o(0), eval(c))
        case until(o, c) => model.absorb(o(0), eval(c))
      }
    }
  }

}
