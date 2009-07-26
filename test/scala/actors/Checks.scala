/*
 * Checks.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package scala.actors

import org.scalacheck._

object Checks extends Properties("String") {
  specify("startsWith", (a: String, b: String) => (a+b).startsWith(a))

  specify("endsWith", (a: String, b: String) => (a+b).endsWith(b))

  // Is this really always true?
  specify("concat", (a: String, b: String) =>
    (a+b).length > a.length && (a+b).length > b.length
  )

  specify("substring", (a: String, b: String) =>
    (a+b).substring(a.length) == b
  )

  specify("substring", (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  )
}