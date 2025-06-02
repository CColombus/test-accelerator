package testaccelerator

import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy.LazyModule  // Add this import for LazyModule

// Configuration fragment to add your accelerator
class WithTestAccelerator extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val testAccelerator = LazyModule(new TestAccelerator(OpcodeSet.custom0)(p))
      testAccelerator
    }
  )
})