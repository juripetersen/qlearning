//> using target { scope "test" }
//> using scala "3.1.3"
//> using lib "org.scalacheck::scalacheck:1.16.0"
//> using lib "org.scalactic::scalactic:3.2.14"
//> using lib "dev.optics::monocle-core:3.1.0"
//> using lib "dev.optics::monocle-macro:3.1.0"

package src

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*

object QLearningSpec
  extends org.scalacheck.Properties("QLearning"):

  property("Ex01.00: No tests now. We test this in Exercise 3") =
    true
