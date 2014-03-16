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

trait ValueProcesses extends Common {

  type RandomVariable[T] = List[T]
  type ValueProcess[T] = Seq[RandomVariable[T]]

  /**
   * Create an infinite stream starting at `start` and incrementing by
   * step `step`.
   *
   * @param start the start value of the stream
   * @param step the increment value of the stream
   * @return the stream starting at value `start`.
   */
  def streamFrom(start: Double, step: Double): Stream[Double] =
    Stream.cons(start, streamFrom(start + step, step))

  final def constProcess[T](x: T): ValueProcess[T] = Stream.continually(List(x))

  implicit class Ops[T](process: ValueProcess[T]) {

    /**
     * Determines the number of time steps in a value process.
     * Only terminates for finite value processes.
     */
    def horizon = process.length

    def mapWith[U](f: T => U): ValueProcess[U] = process.map(_ map f)

    def zipWith[U, V](otherProcess: ValueProcess[U])(f: (T, U) => V): ValueProcess[V] =
      process zip otherProcess map {
        case (rv1, rv2) => rv1 zip rv2 map {
          case (a, b) => f(a, b)
        }
      }
  }

  implicit class BooleanProcess(process: ValueProcess[Boolean]) {

    /**
     * Only terminates for finite value processes.
     * @return True if every value in a value process is true, false otherwise.
     */
    def allTrue: Boolean = process.map(_.forall(x => x)).forall(x => x)
  }

}