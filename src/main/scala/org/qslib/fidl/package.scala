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

package org.qslib

import org.joda.time.DateTime

package object fidl {

  val Now = DateTime.now
  val InfiniteHorizon = new DateTime(10000, 0, 0, 0, 0)

  type Observable = DateTime => Double

  object Implicits extends ObservableIsNumeric with ContractOps

  sealed trait Side {
    def unary_- : Side
  }

  case object Buy extends Side {
    override val unary_- = Sell
  }

  case object Sell extends Side {
    override val unary_- = Buy
  }

}

