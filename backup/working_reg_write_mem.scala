package testaccelerator

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

class TestAccelerator(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new TestAcceleratorModule(this)
}

class TestAcceleratorModule(outer: TestAccelerator)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  val regfile = Reg(Vec(4, UInt(32.W)))
  // val busy = RegInit(VecInit(Seq.fill(4){false.B})) // four busy registers for 4 registers

  // single busy register to indicate if any of the registers are busy
  // val busy = RegInit(false.B)

  val cmd   = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  // val addr = cmd.bits.rs2(log2Up(outer.n)-1,0)

  // define functions for the accelerator
  // val doStart = (funct === 0.U)
  val doAccum = funct === 2.U // load from register file

  // datapath
  val rgPtr      = cmd.bits.rs1                           // pointer to the location in memory
  val rgIdx      = cmd.bits.rs2(log2Up(4) - 1, 0)         // idx of the register to write to
  val memRespTag = io.mem.resp.bits.tag(log2Up(4) - 1, 0) // tag for the memory response

// Add a register to hold the request valid state
  val memReqValid  = RegInit(false.B)
  val memRespValid = RegInit(true.B)

  val reqAddr = Reg(UInt(32.W))
  val reqTag  = Reg(UInt(4.W)) // or whatever width you need
  val reqData = Reg(UInt(32.W))

  val testReg = RegInit(318.asUInt(30.W)) 

  val testValue = Wire(UInt(32.W))
  testValue := testReg

  val setupComplete = RegInit(false.B)
  
// When a command comes in and we're not busy, latch the request
  when(cmd.valid && doAccum && !memReqValid) {
    reqAddr      := rgPtr
    reqTag       := rgIdx
    reqData      := testValue
    printf(cf"*ta*Memory request setup.\n")
    setupComplete := true.B // indicate that setup is complete
    // printf(cf"*ta*Latched memory write request.\n")
  }

  when(cmd.valid && doAccum && setupComplete && !memReqValid) {
    memReqValid  := true.B  // request ready to be sent
    memRespValid := false.B // reponse yet to come
    printf(cf"*ta*Memory request already latched for register $reqTag at address $reqAddr with data $reqData.\n")
  }

// When the request is accepted by memory, clear reqValid and set busy
  when(io.mem.req.fire) {
    memReqValid := false.B // request is sent, clear the valid state
    // busy := true.B
    printf(cf"*ta*Memory request sent for register $reqTag at address $reqAddr.\n")
    // printf(cf"*ta*Request Bits size: ${io.mem.req.bits.size}, data: ${io.mem.req.bits.data}.\n")
  }

// When the response comes back, clear busy
  when(io.mem.resp.valid) {
    // busy := false.B
    memRespValid := true.B // response arrived
    setupComplete := false.B // reset setup complete flag
    testReg := testReg + 3.U // increment the test register
    printf(cf"*ta*Memory response for tag ${io.mem.resp.bits.tag}. Cleared setup flags.\n")
  }

// Memory request interface
  io.mem.req.valid        := memReqValid   // && !busy
  io.mem.req.bits.addr    := reqAddr
  io.mem.req.bits.tag     := reqTag
  io.mem.req.bits.cmd     := M_XWR
  io.mem.req.bits.size    := 2.U //log2Ceil(4).U // 32 -> max 65536
  io.mem.req.bits.signed  := false.B
  io.mem.req.bits.data    := reqData
  io.mem.req.bits.phys    := false.B
  io.mem.req.bits.dprv    := cmd.bits.status.dprv
  io.mem.req.bits.dv      := cmd.bits.status.dv
  io.mem.req.bits.no_resp := false.B

// Ready/response logic
  cmd.ready := !memReqValid

  io.resp.valid   := cmd.valid && !memReqValid && memRespValid
  io.resp.bits.rd := cmd.bits.inst.rd

  io.busy      := memReqValid || !memRespValid
  io.interrupt := false.B
}
