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

trait Models extends ValueProcesses {

  trait Model {
    def valuationDate: Date
    def rateModel: Currency => ValueProcess[Double]
    def exchange(currency1: Currency, currency2: Currency): ValueProcess[Double]
    def absorb(knockOuts: ValueProcess[Boolean], values: ValueProcess[Double]): ValueProcess[Double]
    def discount(currency: Currency): (ValueProcess[Boolean], ValueProcess[Double]) => ValueProcess[Double]
  }

  final class ExampleModel extends Model {
    override val valuationDate = 0

    private def rates(rateNow: Double, delta: Double): ValueProcess[Double] = {

      def rateSlice(minRate: Double, n: Int): RandomVariable[Double] =
        streamFrom(minRate, minRate + (delta * 2)) take n

      def makeRateSlices(rateNow: Double, n: Int): ValueProcess[Double] =
        rateSlice(rateNow, n) +: makeRateSlices(rateNow - delta, n + 1)

      makeRateSlices(rateNow, 1)
    }

    override val rateModel = Map(
      AUD -> rates(15, 1.5),
      CAD -> rates(7, 0.5),
      CHF -> rates(7, 0.8),
      EUR -> rates(6.5, 0.25),
      GBP -> rates(8, 0.5),
      JPY -> rates(11, 1.2),
      USD -> rates(5, 1)
    )

    override def exchange(currency1: Currency, currency2: Currency) = constProcess(1.0)

    override def absorb(knockOuts: ValueProcess[Boolean], values: ValueProcess[Double]) =
      knockOuts.zipWith(values)((knockOut, value) => if (knockOut) 0.0 else value)

    override def discount(currency: Currency) = {
      def prevSlice(slice: RandomVariable[Double]): RandomVariable[Double] =
        if (slice.take(2).length < 2) Nil
        else ((slice(0) + slice(1)) / 2.0) +: prevSlice(slice.tail)

      def discCalc(conditions: ValueProcess[Boolean],
                   values: ValueProcess[Double],
                   rates: ValueProcess[Double]): ValueProcess[Double] = {
        val (bRv, bs) = (conditions.head, conditions.tail)
        val (pRv, ps) = (values.head, values.tail)
        val (rateRv, rs) = (rates.head, rates.tail)

        if (bRv.forall(x => x)) Seq(pRv)
        else {
          val rest = discCalc(bs, ps, rs)
          val nextSlice = rest.head
          val discSlice = prevSlice(nextSlice) zip rateRv map {
            case (x, r) => x / (1 + r)
          }
          val thisSlice = bRv zip pRv zip discSlice map {
            case ((b, p), q) => if (b) p else q
          }
          thisSlice +: rest
        }
      }

      (conditions: ValueProcess[Boolean], values: ValueProcess[Double]) =>
        discCalc(conditions, values, rateModel(currency))
    }
  }

}
