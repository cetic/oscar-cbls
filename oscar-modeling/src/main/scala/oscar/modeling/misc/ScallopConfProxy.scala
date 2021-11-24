/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.misc

import org.rogach.scallop.{ScallopConf, ScallopOption, ValueConverter}

/**
  * A proxy to a ScallopConf (allowing to define "custom" commands with default values)
  */
abstract class ScallopConfProxy {
  private val options = collection.mutable.ArrayBuffer[ScallopConf => Unit]()
  private val optionInstance = collection.mutable.HashMap[(ScallopConf, String), AnyRef]()
  private var proxyTo: ScallopConf = null

  private def shortcut[A](name: String, generateOpt: ScallopConf => ScallopOption[A]): ScallopOption[A] = {
    options += (x => {
      val opt = generateOpt(x)
      optionInstance += ((x, name) -> opt)
    })

    new ScallopOption[A](() => name) {
      override lazy val fn = { name: String =>
        optionInstance((proxyTo, name)).asInstanceOf[ScallopOption[A]].fn(name)
      }
      override lazy val supplied = {
        optionInstance((proxyTo, name)).asInstanceOf[ScallopOption[A]].supplied
      }
    }
  }

  def applyTo(x: ScallopConf) = options.foreach(_(x))
  def setProxyTo(x: ScallopConf) = proxyTo = x

  def opt[A](
              name: String,
              short: Char = '\u0000',
              descr: String = "",
              default: => Option[A] = None,
              validate: A => Boolean = (_:A) => true,
              required: Boolean = false,
              argName: String = "arg",
              hidden: Boolean = false,
              noshort: Boolean = false)
            (implicit conv:ValueConverter[A]): ScallopOption[A] =
    shortcut(name, x => x.opt(name, short, descr, default, validate, required, argName, hidden, noshort)(conv))

  def tally(
             name: String,
             short: Char = '\u0000',
             descr: String = "",
             hidden: Boolean = false,
             noshort: Boolean = false): ScallopOption[Int] =
    shortcut(name, x => x.tally(name, short, descr, hidden, noshort))

  def trailArg[A](
                   name: String,
                   descr: String = "",
                   validate: A => Boolean = (_:A) => true,
                   required: Boolean = true,
                   default: => Option[A] = None,
                   hidden: Boolean = false)
                 (implicit conv:ValueConverter[A]) =
    shortcut(name, x => x.trailArg(name, descr, validate, required, default, hidden)(conv))

  def number(
              name: String,
              descr: String = "",
              validate: Long => Boolean = (_:Long) => true,
              required: Boolean = false,
              default: => Option[Long] = None,
              hidden: Boolean = false)
            (implicit conv: ValueConverter[Long]): ScallopOption[Long] =
    shortcut(name, (x) => x.number(name, descr, validate, required, default, hidden)(conv))

  def toggle(
              name: String,
              default: => Option[Boolean] = None,
              short: Char = '\u0000',
              noshort: Boolean = false,
              prefix: String = "no",
              descrYes: String = "",
              descrNo: String = "",
              hidden: Boolean = false): ScallopOption[Boolean] =
    shortcut(name, x => x.toggle(name, default, short, noshort, prefix, descrYes, descrNo, hidden))
}
