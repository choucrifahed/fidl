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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import java.util.Currency


@RunWith(classOf[JUnitRunner])
class ElementaryContractTest extends Spec with ShouldMatchers {

  val EUR = Currency.getInstance("EUR")

  //  object `Zero Contract` {
  //    object `has by default` {
  //      def `an infinite horizon` {
  //        Zero(EUR).expiry should equal(InfiniteHorizon)
  //      }
  //
  //      def `can be acquired immediately` {
  //        Zero(EUR).start should equal(Today)
  //      }
  //    }
  //
  //    object `should not change` {
  //      def `under give` {
  //        Zero(EUR).give should equal(Zero(EUR))
  //      }
  //
  //      def `under get` {
  //        Zero(EUR).get should equal(Zero(EUR))
  //      }
  //
  //      def `under truncate` {
  //        Zero(EUR).truncate(Today + 1.year) should equal(Zero(EUR))
  //      }
  //
  //      def `under scale` {
  //        Zero(EUR).scale(100.0) should equal(Zero(EUR))
  //      }
  //    }
  //  }
  //
  //  object `One Contract` {
  //    object `has by default` {
  //      def `an infinite horizon` {
  //        One(EUR).expiry should equal(InfiniteHorizon)
  //      }
  //
  //      def `can be acquired immediately` {
  //        One(EUR).start should equal(Today)
  //      }
  //    }
  //
  //    object `when given` {
  //      def `should have side changed` {
  //        One(EUR).give.side should equal(Sell)
  //      }
  //    }
  //
  //    object `on get` {
  //      def `should not change the start date if horizon is infinite` {
  //        One(EUR).get should equal(One(EUR))
  //      }
  //
  //      def `should change the start date if expiry is set` {
  //        val expiry = Today + 1.year
  //        One(EUR).truncate(expiry).get.start should equal(expiry)
  //      }
  //    }
  //
  //    object `when scaled` {
  //      def `should only keep the incoming observable` {
  //        One(EUR).scale(100.0).toString should be("receive 100.0 EUR today")
  //      }
  //    }
  //  }

}
