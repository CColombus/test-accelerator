package testaccelerator

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import chisel3.util.switch
import chisel3.util.is

class TestAccelerator(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new TestAcceleratorModule(this)
}

class TestAcceleratorModule(outer: TestAccelerator)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
    val regfile = Reg(Vec(4, UInt(32.W)))
    val busy = RegInit(VecInit(Seq.fill(4){false.B})) // four busy registers for 4 registers

    val cmd = Queue(io.cmd)
    val funct = cmd.bits.inst.funct
    // val addr = cmd.bits.rs2(log2Up(outer.n)-1,0)

    // define functions for the accelerator
    val doStart = (funct === 0.U)
    val doLoad = (funct === 1.U) // load from register file
    val doAccum = (funct === 2.U) // accumulate to register file

    // datapath
    val rgPtr = cmd.bits.rs1 // pointer to the location in memory
    val rgIdx = cmd.bits.rs2(log2Up(4) - 1, 0) // idx of the register to write to
    //  we will pass rgIdx as the tag to the memory request
    //  and when the response comes back, we can get same tag back
    val memRespTag = io.mem.resp.bits.tag(log2Up(4) - 1, 0) // tag for the memory response
    
    when (io.mem.resp.valid) {
      regfile(memRespTag) := io.mem.resp.bits.data(31,0)
      busy(memRespTag) := false.B
      printf(cf"*ta*Memory response for register ${memRespTag} with data ${io.mem.resp.bits.data(31,0)}.\n")
    }

    when (io.mem.req.fire) {
      printf(cf"*ta*Memory request for register ${rgIdx} at address ${rgPtr}.\n")
      busy(rgIdx) := true.B
    }

    // xd bit indicates whether we need to send a response to the processor
    val doResp = cmd.bits.inst.xd
    // stall since (addr)th register is busy
    val stallReg = busy(rgIdx)
    // stall new load ops since the memory interface is busy with a previous request
    val stallLoad = doLoad && !io.mem.req.ready
    // stall response if the processor is not ready to accept it
    val stallResp = doResp && !io.resp.ready

    cmd.ready := !stallReg && !stallLoad && !stallResp
    // command resolved if no stalls AND not issuing a load that will need a request

    // PROC RESPONSE INTERFACE
      // valid response if valid command, need a response, and no stalls
    io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad
      // Must respond with the appropriate tag or undefined behavior
    io.resp.bits.rd := cmd.bits.inst.rd
      // Send the sum
    val preSum = regfile.reduce(_ + _)

    when (cmd.fire && doAccum) {
      // sum := preSum
      io.resp.bits.data := preSum
      printf(cf"*ta*Accumulater returning sum ${preSum}.\n")
    }


    // busy.reduce(_||_) --> OR of all busy vector registers 
    io.busy := cmd.valid || busy.reduce(_||_)
      // Be busy when have pending memory requests or committed possibility of pending requests
    io.interrupt := false.B
      // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)

    // MEMORY REQUEST INTERFACE
    io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
    io.mem.req.bits.addr := rgPtr
    io.mem.req.bits.tag := rgIdx
    io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
    io.mem.req.bits.size := log2Ceil(4).U // size of the memory request (4 bytes for 32-bit words)
    io.mem.req.bits.signed := false.B
    io.mem.req.bits.data := 0.U // we're not performing any stores...
    io.mem.req.bits.phys := false.B
    io.mem.req.bits.dprv := cmd.bits.status.dprv
    io.mem.req.bits.dv := cmd.bits.status.dv
    io.mem.req.bits.no_resp := false.B
}
