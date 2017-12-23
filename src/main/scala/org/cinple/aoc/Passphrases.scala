package org.cinple.aoc

import scala.io.Source.fromInputStream

object Passphrases extends aoc {

  private val passphraseFn = "/passphrases.txt"

  override protected def part1(): Unit = {
    val stream = getClass.getResourceAsStream(passphraseFn)
    val validPassphrases = fromInputStream(stream).getLines().foldLeft(0) { case (acc, pp) =>
        val spp = pp.split(" ")
        val sspLen = spp.length
        val nspp = spp.distinct
        val nsppLen = nspp.length
        if (sspLen == nsppLen) acc + 1
        else acc
    }
    println(validPassphrases)
  }

  override protected def part2(): Unit = {
    val stream = getClass.getResourceAsStream(passphraseFn)
    val validPassphrases = fromInputStream(stream).getLines().foldLeft(0) { case (acc, pp) =>
      val spp = pp.split(" ")
      val sspLen = spp.length
      val nspp = spp.distinct
      val nsppLen = nspp.length
      if (sspLen == nsppLen) {
       val res = spp.foldLeft(true) { // so hacky
         case (acc0, i) if acc0 =>
           val nnssp = spp.filterNot(_ == i)
           i.permutations.forall(!nnssp.contains(_))
         case (acc0, _) => acc0
       }
       if (res) acc + 1
       else acc
      } else acc
    }
    println(validPassphrases)
  }
}
