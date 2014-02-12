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

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.{Spec, ShouldMatchers}
import com.github.nscala_time.time.Imports._
import Implicits._
import java.util.Currency

@RunWith(classOf[JUnitRunner])
class ContractTreeTest extends Spec with ShouldMatchers {

  val EUR = Currency.getInstance("EUR")

  object `And Combinator` {
    object `when applied to zero` {
      def `should return the other element` {
        Zero(EUR) and One(EUR) should equal(One(EUR))
        One(EUR) and Zero(EUR) should equal(One(EUR))
      }
    }

    object `should propagate` {
      def `give ` {
        give(One(EUR) and One(EUR).give) should equal(One(EUR).give and One(EUR))
      }

      def `get and truncate` {
        val expiry = Today + 1.year
        val result = get(One(EUR) and One(EUR).truncate(expiry)).asInstanceOf[And]
        result.left.asInstanceOf[ElementaryContract].start should be(Today)
        result.right.asInstanceOf[ElementaryContract].start should equal(expiry)
      }

      def `scale ` {
        val input = scale(100.0, One(EUR) and One(EUR).give)
        input.toString should be("(receive 100.0 EUR today) and (pay 100.0 EUR today)")
      }
    }
  }

  object `Or Combinator` {
    object `when applied to zero` {
      def `should return the other element` {
        Zero(EUR) or One(EUR) should equal(One(EUR))
        One(EUR) or Zero(EUR) should equal(One(EUR))
      }
    }

    object `should propagate` {
      def `give ` {
        give(One(EUR) or One(EUR).give) should equal(One(EUR).give or One(EUR))
      }

      def `get and truncate` {
        val expiry = Today + 1.year
        val result = get(One(EUR) or One(EUR).truncate(expiry)).asInstanceOf[Or]
        result.left.asInstanceOf[ElementaryContract].start should be(Today)
        result.right.asInstanceOf[ElementaryContract].start should equal(expiry)
      }

      def `scale ` {
        val input = scale(100.0, One(EUR) or One(EUR).give)
        input.toString should be("(receive 100.0 EUR today) or (pay 100.0 EUR today)")
      }
    }
  }

}
